#!/bin/sh

TGTCLIENT=$1
TGTSESS=$2
TGTFILE=$3

echo "eval -client ${TGTCLIENT} %{ edit $(realpath ${TGTFILE}) }" | kak -p ${TGTSESS}
