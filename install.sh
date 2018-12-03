#!/bin/bash

# Script for symlinking basic config files into the user home directory

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 path/to/dotfiles/git/dir"
  exit 1
fi

GITDIR=$1

# List of files and directories to symlink
declare -a arr=( \
  ".gitignore_global" \
  ".tmux.conf" \
  ".emacs.d/init.el" \
  )

for dotfile in "${arr[@]}"
do
  if [[ -L $HOME/$dotfile ]]; then
    echo "$dotfile already symlinked"
  else
    read -p "Do you wish to symlink $dotfile? [yn] " YN
    case $YN in
    	[Yy]) echo "Setting up $dotfile";;
        *) echo "Skipping $dotfile"; continue;;
    esac
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
