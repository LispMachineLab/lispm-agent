#!/bin/sh
# Record the LispM demo in your own Emacs, with your configuration but
# none of its state: assets/demo/lispm-demo.mp4 and .gif.  macOS only.
#
# A scratch HOME (demo/setup.sh) holds the sample project and an
# .emacs.d made of links to your configuration's code and packages;
# history, recent files, projects and sessions start empty, so nothing
# of yours shows.  Emacs plays demo/record.el while capture-window
# records its frame, and nothing else on the screen (the app running
# this needs Screen Recording access); the model's work is then sped up.
set -eu
demo=$(cd "$(dirname "$0")" && pwd)
repo=$(dirname "$demo")
home=${LISPM_DEMO_HOME:-/tmp/lispm-demo}
config=${LISPM_DEMO_CONFIG:-$HOME/.emacs.d}
emacs=${LISPM_DEMO_EMACS:-/Applications/Emacs.app/Contents/MacOS/Emacs}
out=$repo/assets/demo

# What of the configuration to use: code and packages, never state.
links=${LISPM_DEMO_LINKS:-init.el custom.el lisp elpa eln-cache icons bin tree-sitter native-comp-shim}
# Where the configuration expects a LispM checkout, under HOME.
checkout=${LISPM_DEMO_CHECKOUT:-agents/lisp_machine}

LISPM_DEMO_HOME=$home "$demo/setup.sh"
swiftc -O -swift-version 5 -o "$home/capture-window" "$demo/capture-window.swift"
mkdir -p "$home/.emacs.d" "$home/$(dirname "$checkout")"
for name in $links; do
  if [ -e "$config/$name" ]; then ln -s "$config/$name" "$home/.emacs.d/$name"; fi
done
# The configuration's early init, and an argument for Cocoa: a demo
# covered by other windows must not be put to sleep by App Nap.
cat > "$home/.emacs.d/early-init.el" <<EOI
;;; early-init.el --- Written by demo/record.sh  -*- lexical-binding: t; -*-
(when (file-exists-p "$config/early-init.el")
  (load "$config/early-init.el" nil t))
(add-to-list 'command-line-ns-option-alist '("-NSAppSleepDisabled" 1 ns-ignore-1-arg))
EOI
ln -s "$repo" "$home/$checkout"
# exec-path-from-shell asks a login shell for PATH.
echo "export PATH=\"$PATH\"" > "$home/.zshenv"

# caffeinate: a display that sleeps records nothing.
HOME=$home CFFIXED_USER_HOME=$home LISPM_DEMO_CAPTURE=$home/capture-window \
  LISPM_DEMO_RAW=$home/raw.mov LISPM_DEMO_FILTER=$home/filter.txt \
  caffeinate -d "$emacs" -NSAppSleepDisabled YES -l "$demo/record.el"

mkdir -p "$out"
ffmpeg -hide_banner -loglevel error -y -i "$home/raw.mov" \
  -filter_complex "$(cat "$home/filter.txt")" -map "[out]" \
  -c:v libx264 -preset slow -crf 20 -pix_fmt yuv420p -movflags +faststart \
  "$out/lispm-demo.mp4"
ffmpeg -hide_banner -loglevel error -y -i "$out/lispm-demo.mp4" \
  -vf "fps=15,scale=1280:-1:flags=lanczos,split[a][b];[a]palettegen=stats_mode=diff[p];[b][p]paletteuse=dither=bayer:bayer_scale=5:diff_mode=rectangle" \
  "$out/lispm-demo.gif"
ls -l "$out"
