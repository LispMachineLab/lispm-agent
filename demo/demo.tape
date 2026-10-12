# The LispM demo, recorded with VHS (https://github.com/charmbracelet/vhs).
#
# `make demo' records it again: a real model answers, so it needs the
# default backend (the Rotom gateway) running.  Emacs starts with
# demo/init.el in a scratch HOME prepared by demo/setup.sh.  While the
# model works, the recording pauses (Hide ... Show), so the video shows
# what happened rather than the wait.

Output assets/demo/lispm-demo.gif
Output assets/demo/lispm-demo.mp4

Require emacs
Require git

Set Shell "bash"
Set FontSize 16
Set Width 1280
Set Height 960
Set Padding 18
Set Framerate 24
Set TypingSpeed 45ms
Set WaitTimeout 180s
Set CursorBlink false
Set Theme { "name": "Modus Vivendi Tinted", "background": "#0d0e1c", "foreground": "#ffffff", "cursor": "#ff66ff", "selection": "#555a66", "black": "#0d0e1c", "brightBlack": "#61647a", "red": "#ff5f59", "brightRed": "#ff7f86", "green": "#44bc44", "brightGreen": "#00c06f", "yellow": "#d0bc00", "brightYellow": "#dfaf7a", "blue": "#2fafff", "brightBlue": "#00bcff", "magenta": "#feacd0", "brightMagenta": "#b6a0ff", "cyan": "#00d3d0", "brightCyan": "#6ae4b9", "white": "#c6daff", "brightWhite": "#ffffff" }

Hide
Type "demo/setup.sh && D=$PWD/demo && cd /tmp/lispm-demo/slug && HOME=/tmp/lispm-demo CFFIXED_USER_HOME=/tmp/lispm-demo COLORTERM=truecolor emacs -nw -Q -l $D/init.el slug.el"
Enter
Wait+Screen /slug-from-title/
Sleep 1s
Show

# A small Emacs Lisp project whose tests fail.  Start an agent in it.
Sleep 2s
Escape
Type "x"
Sleep 400ms
Type "lispm"
Sleep 500ms
Enter
Wait+Screen /Welcome to LispM/
Sleep 2s

Type "Run make test, find out why it fails, and fix slug.el."
Sleep 600ms
Enter
Sleep 2s
Hide
Wait+Screen /Answer above with 1, 2 or 3/
Show

# Running a command needs approval: 2 allows it for the rest of the session.
Sleep 3s
Type "2"
Sleep 2s
Hide
Wait+Screen /Answer above with 1, 2 or 3/
Show

# So does an edit, shown as a diff.
Sleep 4s
Type "1"
Sleep 2s
Hide
Wait+Screen /λ assistant@slug is done/
Show
Sleep 4s

# The chat is only the interface: the conversation is an org document,
# and the tool calls are its TODO entries.
Ctrl+c
Ctrl+t
Sleep 1s
Ctrl+x
Type "1"
Escape
Type "<"
Sleep 600ms
Shift+Tab
Shift+Tab
Sleep 6s

# Back in the chat: the agent can drive Emacs itself.
Ctrl+x
Type "b"
Sleep 1s
Enter
Escape
Type "-3"
Ctrl+l
Sleep 1s
Type "Show me the diff of your fix in a window to the right of this chat."
Sleep 600ms
Enter
Sleep 2s
Hide
Wait+Screen /Answer above with 1, 2 or 3/
Show
Sleep 4s
Type "1"
Sleep 1s
Hide
Wait+Screen /λ assistant@slug is done/
Show
Sleep 7s
