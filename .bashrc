# Bash interactive shell settings
# Updated: Feb 15 2016

# NOTE: There are 3 types of bash shells
# - the login shell, which reads ~/.profile
# - the normal shell
# - the interactive shell, which reads ~/.bashrc

# NOTE: Change language settings in ~/.profile and not ~/.bashrc, since if LANG
# is overridden in each subshell multilingual X sessions will not work right

# No core dumps
ulimit -S -c 0

# Done if non-interactive
tty -s || return

# Interactive shell-only from here on
umask 077                       # Strict default file permissions
shopt -s cmdhist                # Save multi-line commands as one command
shopt -s dotglob                # Also glob .files
shopt -s extglob                # Extended globbing
shopt -s globstar               # Enable ** and **/ globbing
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
complete -A directory           mkdir rmdir
complete -A directory           -s default cd

# Environment variables
export EDITOR=vim
export HISTCONTROL=ignoreboth
export HISTIGNORE='&:ls:ll:la:cd:exit:clear:history'
export PATH=$HOME/.local/bin:$PATH
export PS1="[\u@\h:\w]\$ "      # Set prompt

# Load aliases
test -s ~/.alias && . ~/.alias || true
