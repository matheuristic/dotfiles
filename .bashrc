# ~/.bashrc

# NOTE: There are 3 types of bash shells
# - the login shell (reads ~/.bash_profile, ~/.bash_login or ~/.profile on
#   start and ~/.bash_logout on exit)
# - the non-interactive shell (reads the file specified by $BASH_ENV on start)
# - the interactive shell (reads ~/.bashrc on start)

# NOTE: Set language in ~/.bash_profile and not ~/.bashrc, since if LANG is
# overridden in each subshell multilingual X sessions will not work right

# No core dumps
ulimit -S -c 0

# Done if non-interactive
tty -s || return

# Shell options
umask 077                       # Strict default file permissions
shopt -s cmdhist                # Save multi-line commands as one command
shopt -s dotglob                # Also glob .files
shopt -s extglob                # Extended globbing
#shopt -s globstar               # Enable ** and **/ globbing
shopt -s histappend             # Append to history file rather than overwrite

# Auto-completions
complete -A hostname            ping sftp ssh telnet
complete -A export              printenv
complete -A variable            export local readonly unset
complete -A enabled             builtin
complete -A alias               alias unalias
complete -A function            function
complete -A user                su mail finger
complete -A helptopic           help
complete -A shopt               shopt
complete -A stopped -P '%'      bg
complete -A job -P '%'          fg jobs disown
complete -A directory           cd rmdir mkdir

# Environment variables
export EDITOR=vim
export HISTCONTROL=ignoreboth
export HISTFILESIZE=10000
export HISTIGNORE='&:ls:ll:la:cd:exit:clear:history'
export PATH=$HOME/.local/bin:$HOME/.npm_packages/bin:$PATH
export PS1="[\u@\h:\w]\$ "      # Set prompt to "[user@host:/path/to/cwd]$ "

# Load aliases
test -s ~/.alias && . ~/.alias || true

# Load machine-specific bashrc
test -s ~/.bashrc.local && . ~/.bashrc.local || true

# Load machine-specific aliases
test -s ~/.alias.local && . ~/.alias.local || true
