# ~/.config/fish/config.fish - Config file for fish shell
# Author: matheuristic

# Ask before removing replacing existing file
alias rm='rm -i'
alias mv='mv -i'

# Alias 'md5' in OS X to 'md5sum' like in *nix systems
if not type -q md5sum
    alias md5sum='md5'
end
