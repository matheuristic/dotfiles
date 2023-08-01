# ~/.zshrc - ZSH config file, copy this to $HOME/.zshrc and modify as desired

# No core dumps
ulimit -S -c 0

# Done if non-interactive (uncomment next line for slightly faster Zsh scripts)
# tty -s || return

# Default permissions
umask 077

# Use Emacs bindings
bindkey -e

# Autocomplete, also load user-local completions in ~/.zsh_completions
[ -d $HOME/.zsh_completions ] && fpath=($HOME/.zsh_completions $fpath)
autoload -Uz compinit && compinit

# History settings
setopt histignorealldups sharehistory
HISTSIZE=10000
SAVEHIST=10000
#HISTFILE=~/.zsh_history

# Aliases and functions
command -v md5sum > /dev/null \
  || alias md5sum="md5" # alias `md5` to `md5sum` (macOS)
alias map='xargs -n1'   # map fn, e.g. $ find . -name '*.dll' | map dirname
alias dusorted='du -sh * | sort -rh' # sorted disk usage for current directory

# Non-plan9port aliases and functions
if [ -z "$winid" ]; then
  alias ll='ls -lhF'
  alias ls="ls -F"
  alias rm="rm -i"
  lld () {                # directories only
    ls -lhF "$@" | grep --color=never "^d"
  }
  llf () {                # files only
    ls -lhF "$@" | grep --color=never "^-"
  }
fi

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

# Set GnuPG TTY if running in a TTY
test -t 0 && export GPG_TTY=$(tty)

# User-local binaries and manpages
if [ -d $HOME/.local/bin ]; then
  PATH=$HOME/.local/bin:$PATH
fi
if [ -d $HOME/.local/share/man ]; then
  MANPATH=$HOME/.local/share/man:$MANPATH
fi

# Go
if [ -d $HOME/go ]; then
  export GOPATH=$HOME/go
  export PATH=$GOPATH/bin:$PATH
fi

# MacPorts
if [ -d $HOME/macports ]; then
  export PATH=$HOME/macports/bin:$HOME/macports/sbin:$PATH
  export MANPATH=$HOME/macports/share/man:$MANPATH
fi

# plan9port
if [ -d $HOME/.local/plan9 ]; then
  export PLAN9=$HOME/.local/plan9
  export PATH=$PATH:$PLAN9/bin
fi

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
  # Set simple prompt, show last exit code if non-zero
  PROMPT="%(?..[%?] ); "
  RPROMPT=""
fi

# Direnv setup hook
if command -v direnv >/dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi
