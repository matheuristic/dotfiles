#!/bin/sh

# Opens a file in a given Kakoune client and session

CLIENT=$1
SESS=$2
FILE=$3

# Use POSIX solution to get absolute path of file as realpath and readlink are
# GNU coreutils and not POSIX tools, see https://stackoverflow.com/a/3915420
echo "eval -client ${CLIENT} %{ edit $(echo "$(cd "$(dirname "${FILE}")"; pwd -P)/$(basename "${FILE}")") }" | kak -p ${SESS}
