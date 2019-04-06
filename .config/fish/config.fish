# ~/.config/fish/config.fish - Config file for fish shell
# Author: matheuristic

# MacPorts
if test -d /opt/local/bin
    set PATH /opt/local/bin /opt/local/sbin $PATH
end

# Dotnet SDK
if test -d $HOME/dotnet-sdk
    set PATH $HOME/dotnet-sdk $PATH
    set DOTNET_CLI_TELEMETRY_OPTOUT 1
end

# Go
if test -d $HOME/go/bin
    set PATH $HOME/go/bin $PATH
end

# User-installed
if test -d $HOME/.local/bin
    set PATH $HOME/.local/bin $PATH
end

# Conda
if test -f $HOME/miniconda3/etc/fish/conf.d/conda.fish
    source $HOME/miniconda3/etc/fish/conf.d/conda.fish
end

# Bookmarks for cd (add softlinks in the CDPATH directory)
# Directory bookmarks can be prefixed with _ to make them distinct
if test -d $HOME/cdbookmarks
    set -x CDPATH . $HOME/cdbookmarks
end