#!/usr/bin/env sh

# Note than for a typical installation, PLAN9 is usually /usr/local/plan9
export PLAN9=$HOME/packages/plan9port
export PATH=$PATH:$PLAN9/bin

"$PLAN9/bin/rc" "$@"
