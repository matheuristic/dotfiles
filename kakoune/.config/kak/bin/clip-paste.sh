#!/bin/sh

for cmd in `echo "pbcopy wl-copy xclip xsel" | sed -e 's/ +/\n/g'`; do
    if command -v $cmd >/dev/null 2>&1; then clipboardtool=$cmd; break; fi
done
case $clipboardtool in
    pbcopy)  paste="pbpaste";;                   # MacOS
    wl-copy) paste="wl-paste";;                  # Linux (Wayland)
    xclip)   paste="xclip -o";;                  # Linux (X11)
    xsel)    paste="xsel --output --clipboard";; # Linux (X11)
    *)       echo "No supported clipboard tool" >&2; exit 1
esac
$paste "$@"
