// A pseudoterminal for alacritty's event loop that leaves signals alone.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: GPL-3.0-or-later
//
// alacritty_terminal's own PTY learns of its child's exit through a SIGCHLD
// handler, and so does Emacs, each handler calling the one installed before
// it: once both are in one process they call each other until the stack
// overflows.  This PTY installs no handler.  A thread of its own waits for
// the child with waitpid(), reaps it, and pokes a socket the event loop
// polls, as alacritty's SIGCHLD pipe would.
//
// The poke never raises SIGPIPE, whose disposition here is Emacs's: batch
// Emacs leaves it fatal, see `Pty::spawn'.
//
// What the program writes is shown to a watcher as it is read, before
// alacritty parses it, for what alacritty ignores.

use std::fs::File;
use std::io::{self, Read, Write};
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd, RawFd};
use std::os::unix::net::UnixStream;
use std::os::unix::process::{CommandExt, ExitStatusExt};
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};
use std::sync::{Arc, Condvar, Mutex};
use std::time::Duration;

use alacritty_terminal::event::{OnResize, WindowSize};
use alacritty_terminal::tty::{ChildEvent, EventedPty, EventedReadWrite};
use polling::{Event, PollMode, Poller};

// The keys alacritty's event loop tells its PTY's events by, crate-private
// there: the PTY's output, and its child's events.  Cargo.toml pins the
// version they were read from.
const READ_WRITE_KEY: usize = 0;
const CHILD_EVENT_KEY: usize = 1;

/// The child's exit status, once it exited, and a way to wait for it.
#[derive(Default)]
pub struct Exit {
    status: Mutex<Option<ExitStatus>>,
    known: Condvar,
}

impl Exit {
    /// Wait for the child's exit status, at most TIMEOUT.
    pub fn wait(&self, timeout: Duration) -> Option<ExitStatus> {
        let guard = self.status.lock().ok()?;
        let (guard, _) = self.known.wait_timeout_while(guard, timeout, |s| s.is_none()).ok()?;
        *guard
    }
}

/// Shown each read of what the program wrote.
pub type Watch = Box<dyn FnMut(&[u8]) + Send>;

/// The PTY's master as alacritty reads it.
pub struct Output {
    file: File,
    watch: Watch,
}

impl Read for Output {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let read = self.file.read(buf)?;
        (self.watch)(&buf[..read]);
        Ok(read)
    }
}

pub struct Pty {
    output: Output,
    child_events: UnixStream,
    exit: Arc<Exit>,
    pid: i32,
}

fn winsize(size: WindowSize) -> libc::winsize {
    libc::winsize {
        ws_row: size.num_lines,
        ws_col: size.num_cols,
        ws_xpixel: size.num_cols * size.cell_width,
        ws_ypixel: size.num_lines * size.cell_height,
    }
}

fn check(result: libc::c_int) -> io::Result<libc::c_int> {
    if result == -1 { Err(io::Error::last_os_error()) } else { Ok(result) }
}

impl Pty {
    /// Run PROGRAM with ARGS in DIRECTORY with exactly ENVIRONMENT, pairs of
    /// name and value, on a new pseudoterminal of SIZE.  WATCH is shown what
    /// the program writes, as it is read.
    pub fn spawn(
        program: &str,
        args: &[String],
        directory: &Path,
        environment: &[(String, String)],
        size: WindowSize,
        watch: Watch,
    ) -> io::Result<Pty> {
        let (mut master, mut slave): (RawFd, RawFd) = (-1, -1);
        let mut ws = winsize(size);
        check(unsafe {
            libc::openpty(&mut master, &mut slave, std::ptr::null_mut(), std::ptr::null_mut(), &mut ws)
        })?;
        let master = unsafe { OwnedFd::from_raw_fd(master) };
        let slave = unsafe { OwnedFd::from_raw_fd(slave) };

        // Input is UTF-8, so erasing removes a whole character.
        unsafe {
            let mut termios: libc::termios = std::mem::zeroed();
            if libc::tcgetattr(master.as_raw_fd(), &mut termios) == 0 {
                termios.c_iflag |= libc::IUTF8;
                libc::tcsetattr(master.as_raw_fd(), libc::TCSANOW, &termios);
            }
        }

        let mut command = Command::new(program);
        command
            .args(args)
            .current_dir(directory)
            .env_clear()
            .envs(environment.iter().map(|(k, v)| (k, v)))
            .stdin(Stdio::from(slave.try_clone()?))
            .stdout(Stdio::from(slave.try_clone()?))
            .stderr(Stdio::from(slave.try_clone()?));
        let (master_fd, slave_fd) = (master.as_raw_fd(), slave.as_raw_fd());
        unsafe {
            command.pre_exec(move || {
                // A session of its own, the PTY its controlling terminal.
                check(libc::setsid())?;
                check(libc::ioctl(slave_fd, libc::TIOCSCTTY as _, 0))?;
                libc::close(slave_fd);
                libc::close(master_fd);
                // Emacs's signal handling is not the program's.
                for signal in [
                    libc::SIGCHLD, libc::SIGHUP, libc::SIGINT, libc::SIGQUIT, libc::SIGTERM,
                    libc::SIGALRM, libc::SIGPIPE, libc::SIGTSTP, libc::SIGTTIN, libc::SIGTTOU,
                ] {
                    libc::signal(signal, libc::SIG_DFL);
                }
                let mut mask: libc::sigset_t = std::mem::zeroed();
                libc::sigemptyset(&mut mask);
                libc::sigprocmask(libc::SIG_SETMASK, &mask, std::ptr::null_mut());
                Ok(())
            });
        }
        let child = command.spawn()?;
        drop(slave);
        let pid = child.id() as i32;
        // The child is reaped by the waiter below, never by `Child'.
        std::mem::forget(child);

        unsafe {
            let flags = libc::fcntl(master.as_raw_fd(), libc::F_GETFL);
            libc::fcntl(master.as_raw_fd(), libc::F_SETFL, flags | libc::O_NONBLOCK);
        }

        let (child_events, mut poke) = UnixStream::pair()?;
        child_events.set_nonblocking(true)?;
        // A killed terminal drops the PTY, and its end of the socket, before
        // the program it hung up exits, and a write to a socket whose reader
        // is gone raises SIGPIPE.  Interactive Emacs ignores SIGPIPE, but
        // batch Emacs leaves it as it found it, fatal, and the poke would
        // end Emacs.  So the waiter holds the reading end open itself until
        // it poked.
        let reader = child_events.try_clone()?;
        let exit = Arc::new(Exit::default());
        let waiter_exit = exit.clone();
        std::thread::Builder::new().name(format!("lispm-desktop-term wait {pid}")).spawn(move || {
            let mut status: libc::c_int = 0;
            loop {
                let reaped = unsafe { libc::waitpid(pid, &mut status, 0) };
                if reaped == pid {
                    break;
                }
                if reaped == -1 && io::Error::last_os_error().kind() != io::ErrorKind::Interrupted {
                    status = 0;
                    break;
                }
            }
            if let Ok(mut slot) = waiter_exit.status.lock() {
                *slot = Some(ExitStatus::from_raw(status));
            }
            waiter_exit.known.notify_all();
            let _ = poke.write_all(&[1]);
            drop(reader);
        })?;

        Ok(Pty { output: Output { file: File::from(master), watch }, child_events, exit, pid })
    }

    pub fn pid(&self) -> u32 {
        self.pid as u32
    }

    pub fn exit(&self) -> Arc<Exit> {
        self.exit.clone()
    }
}

impl Drop for Pty {
    fn drop(&mut self) {
        // Hang the program up; the waiter reaps it.
        unsafe {
            libc::kill(self.pid, libc::SIGHUP);
        }
    }
}

impl EventedReadWrite for Pty {
    type Reader = Output;
    type Writer = File;

    unsafe fn register(&mut self, poll: &Arc<Poller>, mut interest: Event, mode: PollMode) -> io::Result<()> {
        interest.key = READ_WRITE_KEY;
        unsafe {
            poll.add_with_mode(&self.output.file, interest, mode)?;
            poll.add_with_mode(&self.child_events, Event::readable(CHILD_EVENT_KEY), PollMode::Level)
        }
    }

    fn reregister(&mut self, poll: &Arc<Poller>, mut interest: Event, mode: PollMode) -> io::Result<()> {
        interest.key = READ_WRITE_KEY;
        poll.modify_with_mode(&self.output.file, interest, mode)?;
        poll.modify_with_mode(&self.child_events, Event::readable(CHILD_EVENT_KEY), PollMode::Level)
    }

    fn deregister(&mut self, poll: &Arc<Poller>) -> io::Result<()> {
        poll.delete(&self.output.file)?;
        poll.delete(&self.child_events)
    }

    fn reader(&mut self) -> &mut Output {
        &mut self.output
    }

    fn writer(&mut self) -> &mut File {
        &mut self.output.file
    }
}

impl EventedPty for Pty {
    fn next_child_event(&mut self) -> Option<ChildEvent> {
        let mut byte = [0u8; 1];
        match self.child_events.read(&mut byte) {
            Ok(1) => {
                let status = self.exit.status.lock().ok().and_then(|s| *s);
                Some(ChildEvent::Exited(status))
            }
            _ => None,
        }
    }
}

impl OnResize for Pty {
    fn on_resize(&mut self, size: WindowSize) {
        // A failed resize leaves the old size; it never ends Emacs.
        let ws = winsize(size);
        unsafe {
            libc::ioctl(self.output.file.as_raw_fd(), libc::TIOCSWINSZ, &ws as *const _);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dropping_a_running_program_raises_no_sigpipe() {
        // Test binaries ignore SIGPIPE, batch Emacs does not: a SIGPIPE
        // ends this test binary as it would end Emacs.
        let before = unsafe { libc::signal(libc::SIGPIPE, libc::SIG_DFL) };
        let size = WindowSize { num_lines: 24, num_cols: 80, cell_width: 1, cell_height: 1 };
        let args = ["-c".to_string(), "exec sleep 30".to_string()];
        let environment = [("PATH".to_string(), std::env::var("PATH").unwrap_or_default())];
        let pty = Pty::spawn("/bin/sh", &args, Path::new("/"), &environment, size, Box::new(|_| {}))
            .expect("spawn");
        let exit = pty.exit();
        drop(pty);
        let status = exit.wait(Duration::from_secs(5));
        // The waiter pokes as soon as it knows the exit status.
        std::thread::sleep(Duration::from_millis(200));
        unsafe { libc::signal(libc::SIGPIPE, before) };
        assert_eq!(status.and_then(|s| s.signal()), Some(libc::SIGHUP));
    }
}
