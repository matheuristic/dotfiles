#!/usr/bin/env sh

# Wrapper to launch Acme from environments without Plan 9 binaries in their
# path directories.
 
$HOME/.local/bin/rc.sh $HOME/.local/bin/acme.rc "$@"
