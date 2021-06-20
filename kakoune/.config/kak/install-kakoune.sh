#!/bin/sh

# Build kakoune from source

# Make sure the following packages are installed (Debian, modify for other distros):
# - build-essential
# - libncurses-dev

TAG=$1
PREFIX=${PREFIX:-$HOME/.local}

echo "Cloning source"
git clone https://github.com/mawww/kakoune.git
cd kakoune/src
if [ "${TAG}" != "" ]; then
  echo "Checking out tag: ${TAG}"
  git checkout "tags/${TAG}"
fi

echo "Building binaries and manpages"
make
make man

echo "Installing binaries and manpages to prefix: ${PREFIX}"
PREFIX="${PREFIX}" make install
cd ../..
rm -rf kakoune
echo "Done"
