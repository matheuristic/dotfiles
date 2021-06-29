#!/bin/sh

# Opens a file in a given Kakoune client and session
# Requires realpath which is on Linux systems but which has to be installed on
# MacOS either using MacPorts or Homebrew

TGTCLIENT=$1
TGTSESS=$2
TGTFILE=$3
REALPATHBIN=$(command -v realpath || command -v grealpath)

echo "eval -client ${TGTCLIENT} %{ edit $(${REALPATHBIN} ${TGTFILE}) }" | kak -p ${TGTSESS}
