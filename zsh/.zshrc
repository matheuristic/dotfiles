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

# User-local binaries and manpages
if [ -d $HOME/.local/bin ]; then
  PATH=$HOME/.local/bin:$PATH
fi
if [ -d $HOME/.local/share/man ]; then
  MANPATH=$HOME/.local/share/man:$MANPATH
fi
