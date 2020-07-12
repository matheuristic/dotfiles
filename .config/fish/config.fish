# ~/.config/fish/config.fish - Config file for fish shell
# Author: matheuristic

# MacPorts
if test -d /opt/local/bin
    set PATH /opt/local/bin /opt/local/sbin "$PATH"
end

# Nix (single-user)
# uses the bax command from the fish-bax package
if status --is-login; and test -e "$HOME/.nix-profile/etc/profile.d/nix.sh"
  bax source "$HOME/.nix-profile/etc/profile.d/nix.sh"
end

# Dotnet SDK
if test -d "$HOME/dotnet-sdk"
    set PATH "$HOME/dotnet-sdk" "$PATH"
    set DOTNET_CLI_TELEMETRY_OPTOUT 1
end

# Go
if test -d "$HOME/go/bin"
    set PATH "$HOME/go/bin" "$PATH"
end

# Racket
if test -d "$HOME/racket/bin"
    set PATH "$HOME/racket/bin" "$PATH"
end

# TinyTex
if test -d "$HOME/Library/TinyTeX/bin/x86_64-darwin"
    set PATH "$HOME/Library/TinyTeX/bin/x86_64-darwin" "$PATH"
end

# User-installed
if test -d "$HOME/.local/bin"
    set PATH "$HOME/.local/bin" "$PATH"
end

# Conda
if test -f "$HOME/miniconda3/etc/fish/conf.d/conda.fish"
    source "$HOME/miniconda3/etc/fish/conf.d/conda.fish"
end

# Bookmarks for cd (add softlinks in the CDPATH directory)
# Directory bookmarks can be prefixed with _ to make them distinct
if test -d "$HOME/cdbookmarks"
    set -x CDPATH . "$HOME/cdbookmarks"
end

# Source local aliases
if test -f ~/.config/fish/alias.fish
    source ~/.config/fish/alias.fish
end

# Source local fish config file
if test -f ~/.config/fish/config.fish.local
    source ~/.config/fish/config.fish.local
end

# Redefine the prompt for directory and prompt tracking in emacs-libvterm
# https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
if [ "$INSIDE_EMACS" = "vterm" ]
    functions -c fish_prompt vterm_old_fish_prompt
    function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
        # Remove the trailing newline from the original prompt. This is done
        # using the string builtin from fish, but to make sure any escape codes
        # are correctly interpreted, use %b for printf.
        printf "%b" (string join "\n" (vterm_old_fish_prompt))
        vterm_prompt_end
    end
end
