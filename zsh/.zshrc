# ~/.zshrc - ZSH config file

# No core dumps
ulimit -S -c 0

# Done if non-interactive
tty -s || return

# Set up the prompt
autoload -Uz promptinit
promptinit
prompt redhat

# Default permissions
umask 077

# Use Emacs bindings
bindkey -e

# History settings
setopt histignorealldups sharehistory
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

# Load ZSH-z if installed
if [ -r $HOME/packages/zsh-z/zsh-z.plugin.zsh ]; then
  . $HOME/packages/zsh-z/zsh-z.plugin.zsh
fi

# Completion settings
autoload -Uz compinit && compinit
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,command'

# Set aliases
alias lf='ls -hF'       # list files with type indicators
alias ll='ls -lhF'      # same as above with permissions details
lld () {                # directories only
  ls -lhF "$@" | grep --color=never "^d"
}
alias rm='rm -i'        # remove file with confirmation prompt
command -v md5sum > /dev/null \
  || alias md5sum="md5" # alias `md5` to `md5sum` (macOS)
alias map='xargs -n1'   # map fn, e.g. $ find . -name '*.dll' | map dirname
alias dusorted='du -sh * | sort -rh' # sorted disk usage for current directory

# Load machine-specific config
test -s ~/.zshrc.local && . ~/.zshrc.local || true
