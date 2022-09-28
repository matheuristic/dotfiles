#!/bin/sh

for cmd in `echo "pbcopy wl-copy xclip xsel" | sed -e 's/ +/\n/g'`; do
    if command -v $cmd >/dev/null 2>&1; then clipboardtool=$cmd; break; fi
done
case $clipboardtool in
    pbcopy)  copy="pbcopy";;                   # MacOS
    wl-copy) copy="wl-copy";;                  # Linux (Wayland)
    xclip)   copy="xclip -i";;                 # Linux (X11, xclip)
    xsel)    copy="xsel --input --clipboard";; # Linux (X11, xsel)
    *)       echo "No supported clipboard tool" >&2; exit 1
esac
$copy "$@"
