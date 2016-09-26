#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 path/to/dotfiles/git/dir"
  exit 1
fi

GITDIR=$1

# List of files and directories to symlink
declare -a arr=( \
  ".alias" \
  ".bashrc" \
  ".tmux.conf" \
  ".vimrc" \
  )

for dotfile in "${arr[@]}"
do
  if [[ -L $HOME/$dotfile ]]; then
    echo "$dotfile already symlinked"
  else
    echo "Setting up $dotfile"
    # Backup file
    if [ -f $HOME/$dotfile ]; then
      mv "$HOME/$dotfile" "$HOME/$dotfile.bak"
    fi
    # Symlink new file
    ln -s $GITDIR/$dotfile $HOME/$dotfile
  fi
done
