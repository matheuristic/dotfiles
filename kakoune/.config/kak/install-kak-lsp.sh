#!/bin/sh

# Setup kak-lsp https://github.com/kak-lsp/kak-lsp

# kak-lsp release version
VERSION=${1:-14.1.0}

PREFIX=${PREFIX:-$HOME/.local}
BINDIR=${BINDIR:-$PREFIX/bin}
mkdir -p "${BINDIR}"

case $(uname) in
  Linux)
    ARCH=unknown-linux-musl
    XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
    ;;
  Darwin)
    ARCH=apple-darwin
    # See https://docs.rs/dirs/3.0.2/dirs/fn.config_dir.html
    XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/Library/Preferences}
    ;;
  *)
    echo "unsupported system architecture"
    exit 1
    ;;
esac

CFGDIR="${XDG_CONFIG_HOME}/kak-lsp"
TMPDIR=kak-lsp-v${VERSION}
if [ "$(uname)" = "Linux" ]; then
  TMPFILE=kak-lsp-v${VERSION}-x86_64-${ARCH}.tar.gz
else
  echo 'Only Linux x86_64 is supported' >&2
  exit 1
fi

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
