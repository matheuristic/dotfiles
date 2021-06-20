#!/bin/sh

# Setup plugins

BINDIR=$HOME/.local/bin
mkdir -p "${BINDIR}"

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
CFGDIR="${XDG_CONFIG_HOME}/kak-lsp"

# kak-lsp https://github.com/kak-lsp/kak-lsp
VERSION=10.0.0
case $(uname) in
  Linux)
    ARCH=unknown-linux-musl
    ;;
  Darwin)
    ARCH=apple-darwin
    ;;
  *)
    echo "unsupported system architecture"
    exit 1
    ;;
esac
TMPDIR=kak-lsp-v${VERSION}
TMPFILE=kak-lsp-v${VERSION}-x86_64-${ARCH}.tar.gz
URL="https://github.com/kak-lsp/kak-lsp/releases/download/v${VERSION}/${TMPFILE}"
echo "Create work directory: ${TMPDIR}"
mkdir -p ${TMPDIR}
cd ${TMPDIR}
echo "Downloading kak-lsp release binary from ${URL} to ${TMPFILE}"
wget "${URL}" -O "${TMPFILE}"
echo "Extracting contents of ${TMPFILE}"
tar xzf ${TMPFILE}
echo "Copying kak-lsp binary to ${BINDIR}"
cp -a kak-lsp "$BINDIR"
if [ ! -e "${CFGDIR}/kak-lsp.toml" ]; then
    echo "${CFGDIR}/kak-lsp.toml does not exist. Copying default file"
    mkdir -p "${CFGDIR}"
    cp -a kak-lsp.toml "${CFGDIR}/kak-lsp.toml"
    echo "OK. NOTE: modify ${CFGDIR}/kak-lsp.toml as needed"
fi
cd ..
echo "Removing ${TMPDIR}"
rm -rf ${TMPDIR}
echo "Done"
