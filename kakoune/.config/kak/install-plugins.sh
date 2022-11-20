#!/bin/sh

# Install plugins

install_fzf() {
  # Setup fzf.kak https://github.com/andreyorst/fzf.kak
  VERSION=1.1.0
  URL="https://github.com/andreyorst/fzf.kak/archive/refs/tags/v${VERSION}.tar.gz"
  mkdir -p plugins
  cd plugins
  rm -rf fzf.kak
  wget "$URL" -O fzf-kak.tar.gz
  tar xzf fzf-kak.tar.gz
  mv "fzf.kak-${VERSION}" fzf.kak
  rm -f fzf-kak.tar.gz
  cd ..
  echo 'WARNING: fzf.kak is no longer maintained. Look into replacing with https://github.com/gustavo-hms/peneira'
  return 0
}

install_fzf && echo "Installed fzf.kak" || (echo "Error installing fzf.kak" && exit 1)
