#!/bin/sh

# Download and setup pre-built Helix

# Make sure the following packages are installed:
# - xz

set -e

PREFIX=${PREFIX:-$HOME/.local}
VERSION=22.08.1
OSARCH="$(uname)-$(uname -m)"
case $OSARCH in
	Darwin-arm64)
		ARCHITECTURE='aarch64-macos'
		;;
	Darwin-x86_64)
		ARCHITECTURE='x86_64-macos'
		;;
	Linux-x86_64)
		ARCHITECTURE='x86_64-linux'
		;;
	*)
		echo 'Architecture not supported by install script. Download directly from' >&2
		echo '	https://github.com/helix-editor/helix/releases' >&2
		exit 1
		;;
esac
FILESTEM="helix-${VERSION}-${ARCHITECTURE}"
FILENAME="${FILESTEM}.tar.xz"
URL="https://github.com/helix-editor/helix/releases/download/${VERSION}/${FILENAME}"
TMPDIR=helix-${VERSION}-${ARCHITECTURE}

echo "Changing directory to prefix $PREFIX"
cd "$PREFIX"
echo "Downloading compressed binary archive from $URL"
curl -LO "$URL"
echo "Extracting pre-built Helix into $PREFIX/$FILESTEM"
tar xvJf "$FILENAME"
echo "Creating symlink $PREFIX/bin/hx"
cd "$PREFIX/bin"
rm -f hx
ln -s "../${FILESTEM}/hx"
echo "Removing compressed binary archive"
cd "$PREFIX"
rm -f "${FILENAME}"
echo "Done"
