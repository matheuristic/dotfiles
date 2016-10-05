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
  ".gitignore_global" \
  ".npmrc" \
  ".tmux.conf" \
  ".vimrc" \
  ".emacs.d/init.el" \
  )

for dotfile in "${arr[@]}"
do
  if [[ -L $HOME/$dotfile ]]; then
    echo "$dotfile already symlinked"
  else
    echo "Setting up $dotfile"
    # Create directory for symlink if necessary
    mkdir -p `dirname $HOME/$dotfile`
    if [ $? -ne 0 ]; then
        echo "unable to create base dir for symlink, skipping file"
        continue
    fi
    # Backup file
    if [ -f $HOME/$dotfile ]; then
      mv "$HOME/$dotfile" "$HOME/$dotfile.bak"
    fi
    # Create symlink
    ln -s $GITDIR/$dotfile $HOME/$dotfile
  fi
done
