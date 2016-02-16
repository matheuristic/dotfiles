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

umask 077                       # Strict default file permissions
export PS1="[\u@\h:\w]\$ "      # Set prompt if interactive
shopt -s extglob                # Extended globbing

# Auto-completions
complete -A hostname            rsh rcp telnet rlogin ftp ping disk ssh scp git rsync
complete -A export              printenv
complete -A variable            export local readonly unset
complete -A enabled             builtin
complete -A alias               alias unalias
complete -A function            function
complete -A user                su mail finger
complete -A helptopic           help  # Currently same as builtins.
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
export PYTHONPATH=$HOME/.local/lib/python3.4/site-packages:$PTYHONPATH

# Load aliases
test -s ~/.alias && . ~/.alias || true
