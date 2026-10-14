// lme-term: terminals for lispm-emacs, emulated off Emacs's thread.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Emacs runs all of its Lisp, process output and redisplay on one thread,
// so a terminal emulated in Lisp makes every terminal's output, shown or
// not, wait in line with the keyboard.  Here each terminal gets a thread of
// its own: it reads the program's output from the PTY and keeps the screen
// up to date (alacritty_terminal does the emulation), whether or not Emacs
// shows it.  Emacs is woken only for terminals it shows, at most once
// between two drawings, and then copies only the lines that changed:
//
//   program --PTY--> terminal's thread: parse, update the screen
//            |  shown and changed?  one line "ID\n" on the channel
//            v
//   Emacs (lme-term.el): render the changed lines into the buffer
//
// Titles, exits and clipboard writes reach Emacs whether shown or not.

mod pty;

use std::borrow::Cow;
use std::io::Write;
use std::path::Path;
use std::time::Duration;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU32, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use alacritty_terminal::event::{Event, EventListener, WindowSize};
use alacritty_terminal::event_loop::{EventLoop, EventLoopSender, Msg};
use alacritty_terminal::grid::{Dimensions, Scroll};
use alacritty_terminal::index::{Column, Line};
use alacritty_terminal::sync::FairMutex;
use alacritty_terminal::term::cell::Flags;
use alacritty_terminal::term::{Config, Term, TermDamage, TermMode};
use alacritty_terminal::vte::ansi::{Color, CursorShape, NamedColor, Rgb};
use emacs::{defun, Env, IntoLisp, Result, Value, Vector};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "lme-term-module", defun_prefix = "lme-term-module", separator = "-")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

/// The pipe that wakes Emacs: lines of terminal ids, written from any thread.
static CHANNEL: OnceLock<Mutex<Box<dyn Write + Send>>> = OnceLock::new();

static NEXT_ID: AtomicU32 = AtomicU32::new(1);

/// The terminal's size, as alacritty's grid takes it.
struct Size {
    columns: usize,
    lines: usize,
}

impl Dimensions for Size {
    fn total_lines(&self) -> usize {
        self.lines
    }
    fn screen_lines(&self) -> usize {
        self.lines
    }
    fn columns(&self) -> usize {
        self.columns
    }
}

/// What happened in a terminal that Emacs must hear of, shown or not.
enum Happening {
    Title(String),
    ResetTitle,
    Exit(i32),
    Clipboard(String),
    Bell,
}

/// State shared by a terminal's thread and Emacs.
struct Shared {
    id: u32,
    /// Emacs shows the terminal: only then is it woken for new output.
    visible: AtomicBool,
    /// Emacs was woken and has not drawn the terminal since.
    woken: AtomicBool,
    /// The next drawing redraws every line.
    full: AtomicBool,
    alive: AtomicBool,
    exit_code: AtomicI32,
    happenings: Mutex<Vec<Happening>>,
    sender: OnceLock<EventLoopSender>,
    /// The colours answered when a program asks: the palette, then the
    /// default foreground and background.
    palette: Mutex<Vec<Rgb>>,
    size: Mutex<WindowSize>,
}

impl Shared {
    /// Wake Emacs for this terminal, once until it draws it.
    fn wake(&self) {
        if !self.woken.swap(true, Ordering::AcqRel) {
            if let Some(channel) = CHANNEL.get() {
                if let Ok(mut channel) = channel.lock() {
                    let _ = writeln!(channel, "{}", self.id);
                    let _ = channel.flush();
                }
            }
        }
    }

    fn happen(&self, happening: Happening) {
        if let Ok(mut happenings) = self.happenings.lock() {
            happenings.push(happening);
        }
        // Emacs must hear of it even while the terminal is hidden.
        self.woken.store(false, Ordering::Release);
        self.wake();
    }

    fn write(&self, bytes: Vec<u8>) {
        if let Some(sender) = self.sender.get() {
            let _ = sender.send(Msg::Input(Cow::Owned(bytes)));
        }
    }

    fn color(&self, index: usize) -> Rgb {
        let palette = self.palette.lock().map(|p| p.clone()).unwrap_or_default();
        let fallback = Rgb { r: 0, g: 0, b: 0 };
        match index {
            i if i == NamedColor::Foreground as usize => palette.get(16).copied().unwrap_or(fallback),
            i if i == NamedColor::Background as usize => palette.get(17).copied().unwrap_or(fallback),
            i if i < 16 => palette.get(i).copied().unwrap_or_else(|| xterm_color(i as u8)),
            i if i < 256 => xterm_color(i as u8),
            _ => palette.get(16).copied().unwrap_or(fallback),
        }
    }
}

/// The colour of xterm's 256-colour palette at INDEX.
fn xterm_color(index: u8) -> Rgb {
    const BASE: [(u8, u8, u8); 16] = [
        (0, 0, 0), (205, 0, 0), (0, 205, 0), (205, 205, 0),
        (0, 0, 238), (205, 0, 205), (0, 205, 205), (229, 229, 229),
        (127, 127, 127), (255, 0, 0), (0, 255, 0), (255, 255, 0),
        (92, 92, 255), (255, 0, 255), (0, 255, 255), (255, 255, 255),
    ];
    match index {
        0..=15 => {
            let (r, g, b) = BASE[index as usize];
            Rgb { r, g, b }
        }
        16..=231 => {
            let i = index - 16;
            let level = |v: u8| if v == 0 { 0 } else { 55 + v * 40 };
            Rgb { r: level(i / 36), g: level((i / 6) % 6), b: level(i % 6) }
        }
        _ => {
            let v = 8 + (index - 232) * 10;
            Rgb { r: v, g: v, b: v }
        }
    }
}

#[derive(Clone)]
struct Listener(Arc<Shared>);

impl EventListener for Listener {
    fn send_event(&self, event: Event) {
        let shared = &self.0;
        match event {
            Event::Wakeup => {
                if shared.visible.load(Ordering::Acquire) {
                    shared.wake();
                }
            }
            Event::PtyWrite(text) => shared.write(text.into_bytes()),
            Event::Title(title) => shared.happen(Happening::Title(title)),
            Event::ResetTitle => shared.happen(Happening::ResetTitle),
            Event::ClipboardStore(_, text) => shared.happen(Happening::Clipboard(text)),
            Event::Bell => shared.happen(Happening::Bell),
            Event::ColorRequest(index, format) => {
                let rgb = shared.color(index);
                shared.write(format(rgb).into_bytes());
            }
            Event::TextAreaSizeRequest(format) => {
                if let Ok(size) = shared.size.lock() {
                    shared.write(format(*size).into_bytes());
                }
            }
            // The exit is reported once the last output was read, see `spawn'.
            _ => {}
        }
    }
}

/// A terminal: its screen, the thread running its program, and the channel
/// to that thread.
struct Terminal {
    shared: Arc<Shared>,
    term: Arc<FairMutex<Term<Listener>>>,
    sender: EventLoopSender,
    pid: u32,
}

impl Drop for Terminal {
    fn drop(&mut self) {
        // The thread drops the PTY, which hangs the program up.
        let _ = self.sender.send(Msg::Shutdown);
    }
}

fn window_size(columns: u16, lines: u16) -> WindowSize {
    WindowSize { num_lines: lines.max(1), num_cols: columns.max(1), cell_width: 1, cell_height: 1 }
}

fn strings(vector: Vector) -> Result<Vec<String>> {
    (0..vector.len()).map(|i| vector.get::<String>(i)).collect()
}

/// Wake Emacs through PROCESS, a pipe process, from now on.
#[defun]
fn init_channel(env: &Env, process: Value) -> Result<()> {
    let writer = env.open_channel(process)?;
    let _ = CHANNEL.set(Mutex::new(Box::new(writer)));
    Ok(())
}

/// Run PROGRAM with ARGS, a vector, in DIRECTORY with exactly the environment
/// ENVIRONMENT, a vector of "NAME=VALUE", on a terminal of COLUMNS by LINES.
/// Return the terminal.
#[defun(user_ptr)]
fn spawn(
    program: String,
    args: Vector,
    directory: String,
    environment: Vector,
    columns: u16,
    lines: u16,
) -> Result<Terminal> {
    let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
    let size = window_size(columns, lines);
    // The program sees exactly Emacs's `process-environment', not the
    // environment the Emacs process was started with.
    let environment: Vec<(String, String)> = strings(environment)?
        .into_iter()
        .filter_map(|entry| entry.split_once('=').map(|(k, v)| (k.to_string(), v.to_string())))
        .collect();
    let shared = Arc::new(Shared {
        id,
        visible: AtomicBool::new(false),
        woken: AtomicBool::new(false),
        full: AtomicBool::new(true),
        alive: AtomicBool::new(true),
        exit_code: AtomicI32::new(0),
        happenings: Mutex::new(Vec::new()),
        sender: OnceLock::new(),
        palette: Mutex::new(Vec::new()),
        size: Mutex::new(size),
    });
    let listener = Listener(shared.clone());
    let config = Config { scrolling_history: 10_000, ..Default::default() };
    let term = Arc::new(FairMutex::new(Term::new(
        config,
        &Size { columns: size.num_cols as usize, lines: size.num_lines as usize },
        listener.clone(),
    )));
    let pty = pty::Pty::spawn(&program, &strings(args)?, Path::new(&directory), &environment, size)?;
    let pid = pty.pid();
    let exit = pty.exit();
    let event_loop = EventLoop::new(term.clone(), listener, pty, true, false)?;
    let sender = event_loop.channel();
    let _ = shared.sender.set(sender.clone());
    let running = event_loop.spawn();
    // Once the loop ended, every byte of output was read: the PTY is
    // dropped, hanging the program up if it still ran, and its exit reported.
    let watched = shared.clone();
    std::thread::Builder::new().name(format!("lme-term watch {id}")).spawn(move || {
        drop(running.join());
        let code = exit.wait(Duration::from_secs(10)).and_then(|s| s.code()).unwrap_or(-1);
        watched.exit_code.store(code, Ordering::Release);
        watched.alive.store(false, Ordering::Release);
        watched.happen(Happening::Exit(code));
    })?;
    Ok(Terminal { shared, term, sender, pid })
}

/// Return TERMINAL's id, as the channel names it.
#[defun]
fn id(terminal: &Terminal) -> Result<u32> {
    Ok(terminal.shared.id)
}

/// Return the process id of TERMINAL's program.
#[defun]
fn pid(terminal: &Terminal) -> Result<u32> {
    Ok(terminal.pid)
}

/// Return non-nil while TERMINAL's program runs.
#[defun]
fn alive_p(terminal: &Terminal) -> Result<bool> {
    Ok(terminal.shared.alive.load(Ordering::Acquire))
}

/// Send STRING to TERMINAL's program, as typed; show the newest output.
#[defun]
fn input(terminal: &Terminal, string: String) -> Result<()> {
    if string.is_empty() {
        return Ok(());
    }
    {
        let mut term = terminal.term.lock();
        if term.grid().display_offset() != 0 {
            term.scroll_display(Scroll::Bottom);
            terminal.shared.full.store(true, Ordering::Release);
        }
    }
    let _ = terminal.sender.send(Msg::Input(Cow::Owned(string.into_bytes())));
    Ok(())
}

/// Make TERMINAL COLUMNS by LINES.
#[defun]
fn resize(terminal: &Terminal, columns: u16, lines: u16) -> Result<()> {
    let size = window_size(columns, lines);
    if let Ok(mut current) = terminal.shared.size.lock() {
        if current.num_cols == size.num_cols && current.num_lines == size.num_lines {
            return Ok(());
        }
        *current = size;
    }
    terminal
        .term
        .lock()
        .resize(Size { columns: size.num_cols as usize, lines: size.num_lines as usize });
    let _ = terminal.sender.send(Msg::Resize(size));
    terminal.shared.full.store(true, Ordering::Release);
    Ok(())
}

/// Tell TERMINAL whether Emacs shows it: VISIBLE.  A terminal just shown
/// is redrawn whole, and woken at once.
#[defun]
fn set_visible(terminal: &Terminal, visible: Option<Value>) -> Result<()> {
    let visible = visible.is_some();
    let was = terminal.shared.visible.swap(visible, Ordering::AcqRel);
    if visible && !was {
        terminal.shared.full.store(true, Ordering::Release);
        terminal.shared.woken.store(false, Ordering::Release);
        terminal.shared.wake();
    }
    Ok(())
}

/// Set the colours TERMINAL answers programs asking: COLORS, a vector of 18
/// "#RRGGBB" strings, the 16 of the palette then foreground and background.
#[defun]
fn set_colors(terminal: &Terminal, colors: Vector) -> Result<()> {
    let parse = |s: &str| -> Option<Rgb> {
        let hex = s.strip_prefix('#')?;
        if hex.len() != 6 {
            return None;
        }
        let v = u32::from_str_radix(hex, 16).ok()?;
        Some(Rgb { r: (v >> 16) as u8, g: (v >> 8) as u8, b: v as u8 })
    };
    let palette: Vec<Rgb> = strings(colors)?
        .iter()
        .enumerate()
        .map(|(i, s)| parse(s).unwrap_or_else(|| xterm_color(i.min(255) as u8)))
        .collect();
    if let Ok(mut current) = terminal.shared.palette.lock() {
        *current = palette;
    }
    Ok(())
}

/// Scroll TERMINAL's view by LINES into its history (up when positive), or
/// back to the newest output when LINES is nil.  Return how far back it is.
#[defun]
fn scroll(terminal: &Terminal, lines: Option<i32>) -> Result<usize> {
    let mut term = terminal.term.lock();
    term.scroll_display(match lines {
        Some(lines) => Scroll::Delta(lines),
        None => Scroll::Bottom,
    });
    terminal.shared.full.store(true, Ordering::Release);
    Ok(term.grid().display_offset())
}

/// Return the modes of TERMINAL Emacs's keys depend on, as a bit set:
/// 1 cursor keys in application mode, 2 bracketed paste, 4 the alternate
/// screen, 8 the program wants mouse reports, 16 in SGR form, 32 focus
/// reports, 64 keypad in application mode, 128 it wants the mouse's moves
/// while a button is held, 256 all its moves.
#[defun]
fn modes(terminal: &Terminal) -> Result<u32> {
    let mode = *terminal.term.lock().mode();
    let mut bits = 0;
    for (flag, bit) in [
        (TermMode::APP_CURSOR, 1),
        (TermMode::BRACKETED_PASTE, 2),
        (TermMode::ALT_SCREEN, 4),
        (TermMode::MOUSE_MODE, 8),
        (TermMode::SGR_MOUSE, 16),
        (TermMode::FOCUS_IN_OUT, 32),
        (TermMode::APP_KEYPAD, 64),
        (TermMode::MOUSE_DRAG, 128),
        (TermMode::MOUSE_MOTION, 256),
    ] {
        if mode.intersects(flag) {
            bits |= bit;
        }
    }
    Ok(bits)
}

/// Return what happened in TERMINAL since last asked, oldest first, as a
/// vector of (KIND . VALUE): (title . STRING), (reset-title), (exit . CODE),
/// (clipboard . STRING), (bell).
#[defun]
fn happenings<'e>(env: &'e Env, terminal: &Terminal) -> Result<Value<'e>> {
    let taken: Vec<Happening> = match terminal.shared.happenings.lock() {
        Ok(mut happenings) => std::mem::take(&mut *happenings),
        Err(_) => Vec::new(),
    };
    let mut items = Vec::with_capacity(taken.len());
    for happening in taken {
        let (kind, value) = match happening {
            Happening::Title(title) => ("title", title.into_lisp(env)?),
            Happening::ResetTitle => ("reset-title", ().into_lisp(env)?),
            Happening::Exit(code) => ("exit", code.into_lisp(env)?),
            Happening::Clipboard(text) => ("clipboard", text.into_lisp(env)?),
            Happening::Bell => ("bell", ().into_lisp(env)?),
        };
        items.push(env.cons(env.intern(kind)?, value)?);
    }
    env.call("vector", &items[..])
}

/// A face code for a cell's colour: -1 for the default, 0 to 255 for the
/// palette, #x1000000 plus RGB for an exact colour.
fn color_code(color: Color) -> i64 {
    match color {
        Color::Spec(Rgb { r, g, b }) => 0x1000000 | ((r as i64) << 16) | ((g as i64) << 8) | b as i64,
        Color::Indexed(index) => index as i64,
        Color::Named(named) => {
            let n = named as usize;
            if n < 16 {
                n as i64
            } else {
                match named {
                    NamedColor::DimBlack => 0,
                    NamedColor::DimRed => 1,
                    NamedColor::DimGreen => 2,
                    NamedColor::DimYellow => 3,
                    NamedColor::DimBlue => 4,
                    NamedColor::DimMagenta => 5,
                    NamedColor::DimCyan => 6,
                    NamedColor::DimWhite => 7,
                    _ => -1,
                }
            }
        }
    }
}

/// A face code for a cell's flags: 1 bold, 2 italic, 4 underline, 8 inverse,
/// 16 dim, 32 strike-through, 64 curly or double underline.
fn flags_code(flags: Flags) -> i64 {
    let mut code = 0;
    for (flag, bit) in [
        (Flags::BOLD, 1),
        (Flags::ITALIC, 2),
        (Flags::UNDERLINE | Flags::DOTTED_UNDERLINE | Flags::DASHED_UNDERLINE, 4),
        (Flags::INVERSE, 8),
        (Flags::DIM, 16),
        (Flags::STRIKEOUT, 32),
        (Flags::UNDERCURL | Flags::DOUBLE_UNDERLINE, 64),
    ] {
        if flags.intersects(flag) {
            code |= bit;
        }
    }
    code
}

/// Return TERMINAL's changed lines since it was last drawn, all of them
/// when FULL or after a resize, scroll or showing:
/// [LINES CURSOR-LINE CURSOR-COLUMN CURSOR-SHAPE CHANGED], where LINES is
/// the screen's height, CURSOR-SHAPE 0 hidden, 1 block, 2 underline, 3 bar,
/// 4 hollow block, and CHANGED a vector of [LINE TEXT RUNS], RUNS a vector
/// of START END FOREGROUND BACKGROUND FLAGS repeated, see `color_code' and
/// `flags_code'; trailing blanks in the default colours are left out, and
/// so are runs in the default colours.
#[defun]
fn render<'e>(env: &'e Env, terminal: &Terminal, full: Option<Value>) -> Result<Value<'e>> {
    let full = full.is_some() || terminal.shared.full.swap(false, Ordering::AcqRel);
    // From here on new output wakes Emacs again.
    terminal.shared.woken.store(false, Ordering::Release);
    let mut term = terminal.term.lock();
    let lines = term.screen_lines();
    let columns = term.columns();
    let damaged: Vec<usize> = if full {
        (0..lines).collect()
    } else {
        match term.damage() {
            TermDamage::Full => (0..lines).collect(),
            TermDamage::Partial(iter) => {
                let mut v: Vec<usize> = iter.map(|d| d.line).filter(|&l| l < lines).collect();
                v.sort_unstable();
                v.dedup();
                v
            }
        }
    };
    term.reset_damage();
    let grid = term.grid();
    let offset = grid.display_offset() as i32;
    let mut changed = Vec::with_capacity(damaged.len());
    for index in damaged {
        let row = &grid[Line(index as i32 - offset)];
        let mut text = String::with_capacity(columns);
        let mut runs: Vec<i64> = Vec::new();
        let mut chars = 0i64;
        // The run being gathered: its start, colours and flags.
        let mut run: Option<(i64, i64, i64, i64)> = None;
        let mut last_visible = 0i64;
        let mut last_visible_byte = 0usize;
        for column in 0..columns {
            let cell = &row[Column(column)];
            if cell.flags.intersects(Flags::WIDE_CHAR_SPACER | Flags::LEADING_WIDE_CHAR_SPACER) {
                continue;
            }
            let fg = color_code(cell.fg);
            let bg = color_code(cell.bg);
            let flags = flags_code(cell.flags);
            let c = if cell.flags.contains(Flags::HIDDEN) { ' ' } else { cell.c };
            match run {
                Some((start, f, b, fl)) if (f, b, fl) != (fg, bg, flags) => {
                    runs.extend_from_slice(&[start, chars, f, b, fl]);
                    run = Some((chars, fg, bg, flags));
                }
                None => run = Some((chars, fg, bg, flags)),
                _ => {}
            }
            text.push(c);
            chars += 1;
            if let Some(extra) = cell.zerowidth() {
                for z in extra {
                    text.push(*z);
                    chars += 1;
                }
            }
            if c != ' ' || bg != -1 || flags & (4 | 8 | 32 | 64) != 0 {
                last_visible = chars;
                last_visible_byte = text.len();
            }
        }
        if let Some((start, f, b, fl)) = run {
            runs.extend_from_slice(&[start, chars, f, b, fl]);
        }
        // Leave out the blank tail, and the runs past it or in the defaults.
        text.truncate(last_visible_byte);
        let mut kept: Vec<Value> = Vec::with_capacity(runs.len());
        for run in runs.chunks(5) {
            let (start, end) = (run[0], run[1].min(last_visible));
            if start >= end || (run[2] == -1 && run[3] == -1 && run[4] == 0) {
                continue;
            }
            for v in [start, end, run[2], run[3], run[4]] {
                kept.push(v.into_lisp(env)?);
            }
        }
        let runs = env.call("vector", &kept[..])?;
        let entry = [(index as i64).into_lisp(env)?, text.into_lisp(env)?, runs];
        changed.push(env.call("vector", &entry[..])?);
    }
    let cursor = grid.cursor.point;
    let shape: i64 = if !term.mode().contains(TermMode::SHOW_CURSOR) {
        0
    } else {
        match term.cursor_style().shape {
            CursorShape::Block => 1,
            CursorShape::Underline => 2,
            CursorShape::Beam => 3,
            CursorShape::HollowBlock => 4,
            CursorShape::Hidden => 0,
        }
    };
    let cursor_line = cursor.line.0 as i64 + offset as i64;
    let shape = if cursor_line >= lines as i64 { 0 } else { shape };
    let changed = env.call("vector", &changed[..])?;
    let result = [
        (lines as i64).into_lisp(env)?,
        cursor_line.into_lisp(env)?,
        (cursor.column.0 as i64).into_lisp(env)?,
        shape.into_lisp(env)?,
        changed,
    ];
    env.call("vector", &result[..])
}

/// Return up to LINES lines of TERMINAL's history and screen as one string,
/// oldest first, for reading and searching in a buffer of its own.
#[defun]
fn text(terminal: &Terminal, lines: Option<usize>) -> Result<String> {
    let term = terminal.term.lock();
    let grid = term.grid();
    let history = grid.history_size();
    let screen = term.screen_lines();
    let total = history + screen;
    let wanted = lines.unwrap_or(total).min(total);
    let mut out = String::new();
    for i in (total - wanted)..total {
        let row = &grid[Line(i as i32 - history as i32)];
        let mut text = String::new();
        for column in 0..term.columns() {
            let cell = &row[Column(column)];
            if cell.flags.intersects(Flags::WIDE_CHAR_SPACER | Flags::LEADING_WIDE_CHAR_SPACER) {
                continue;
            }
            text.push(cell.c);
            if let Some(extra) = cell.zerowidth() {
                text.extend(extra.iter());
            }
        }
        out.push_str(text.trim_end());
        out.push('\n');
    }
    Ok(out)
}

/// Stop TERMINAL's program: its thread hangs it up and ends.
#[defun]
fn kill(terminal: &Terminal) -> Result<()> {
    let _ = terminal.sender.send(Msg::Shutdown);
    Ok(())
}
