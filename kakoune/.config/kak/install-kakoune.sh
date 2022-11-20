#!/bin/sh

# Build kakoune from source

# Make sure the following packages are installed (Debian, modify for other distros):
# - build-essential
# - libncurses-dev

# Kakoune release version
VERSION=${1:-2022.10.31}

PREFIX=${PREFIX:-$HOME/.local}

URL="https://github.com/mawww/kakoune/releases/download/v${VERSION}/kakoune-${VERSION}.tar.bz2"
TMPDIR=kakoune-${VERSION}
TMPFILE=kakoune-${VERSION}.tar.bz2

echo "Downloading source for release v${VERSION} to ${TMPFILE}"
wget "${URL}" -O "${TMPFILE}"
echo "Expanding ${TMPFILE}"
tar xjf "${TMPFILE}"
echo "Entering ${TMPDIR}/src"
cd "${TMPDIR}/src"

echo "Building binaries and manpages"
make
make man

echo "Installing binaries and manpages to prefix: ${PREFIX}"
PREFIX="${PREFIX}" make install
cd ../..
echo "Removing work directory and source"
rm -rf "${TMPDIR}" "${TMPFILE}"
echo "Done"
