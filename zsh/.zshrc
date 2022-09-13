# ~/.zshrc - ZSH config file, copy this to $HOME/.zshrc and modify as desired

# No core dumps
ulimit -S -c 0

# Done if non-interactive (uncomment next line for slightly faster Zsh scripts)
# tty -s || return

# Default permissions
umask 077

# Use Emacs bindings
bindkey -e

# History settings
setopt histignorealldups sharehistory
HISTSIZE=10000
SAVEHIST=10000
# HISTFILE=~/.zsh_history

# Aliases and functions
alias ll='ls -lhF'      # same as above with permissions details
lld () {                # directories only
  ls -lhF "$@" | grep --color=never "^d"
}
llf () {                # files only
  ls -lhF "$@" | grep --color=never "^-"
}
alias rm='rm -i'        # remove file with confirmation prompt
command -v md5sum > /dev/null \
  || alias md5sum="md5" # alias `md5` to `md5sum` (macOS)
alias map='xargs -n1'   # map fn, e.g. $ find . -name '*.dll' | map dirname
alias dusorted='du -sh * | sort -rh' # sorted disk usage for current directory

# Prevent Mac from sleeping for given time: $ keepawake <hrs>
if [[ "x`uname`" == "xDarwin" ]]; then
  keepawake () {
    KEEPAWAKEHRS=${1:-8}  # default 8 hours
    KEEPAWAKESECS=$(( 60 * 60 * KEEPAWAKEHRS ))
    echo "keeping machine awake for $KEEPAWAKEHRS hours"
    echo "start : `date`"
    echo "end   : `date -v +${KEEPAWAKEHRS}H`"
    caffeinate -i -t $KEEPAWAKESECS
  }
fi

# Set paths as needed

# go support
if [ -d $HOME/go/bin ]; then
  export PATH=$HOME/go/bin:$PATH
fi

# User-local binaries and manpages
if [ -d $HOME/.local/bin ]; then
  PATH=$HOME/.local/bin:$PATH
fi
if [ -d $HOME/.local/share/man ]; then
  MANPATH=$HOME/.local/share/man:$MANPATH
fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/thiam/mambaforge/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/thiam/mambaforge/etc/profile.d/conda.sh" ]; then
        . "/home/thiam/mambaforge/etc/profile.d/conda.sh"
    else
        export PATH="/home/thiam/mambaforge/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/home/thiam/mambaforge/etc/profile.d/mamba.sh" ]; then
    . "/home/thiam/mambaforge/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<

# Update Acme window tag line with dir in which it's running
if [ "$winid" ]; then
    _acme_cd () {
        builtin cd "$@" && awd
    }
    alias cd=_acme_cd
fi

# No fancy Zsh prompt when using dumb terminals
if [[ "$TERM" == "dumb" ]]; then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  if whence -w precmd >/dev/null; then
      unfunction precmd
  fi
  if whence -w preexec >/dev/null; then
      unfunction preexec
  fi
  PROMPT="%# "
  RPROMPT=""
fi
