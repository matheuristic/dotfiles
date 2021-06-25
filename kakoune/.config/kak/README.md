# Kakoune configuration

## MacOS instructions

The Rust `dirs::config_dir` function yields `$XDG_CONFIG_HOME` on Linux
(which is `$HOME/.config`) but is `$HOME/Library/Preferences` on MacOS.

This determines where the `kak-lsp` module will look for its config file,
so on MacOS make sure to symlink the `kak-lsp.toml` file to the appropriate
location. The following code block assumes that file is installed to
`$HOME/.config/kak-lsp/`.

```shell
mkdir -p $HOME/Library/Preferences
ln -s $HOME/.config/kak-lsp/kak-lsp.toml $HOME/Library/Preferences/kak-lsp.toml
```

## Installing the editor from source

Install the next packages (Debian, modify as needed):

- `build-essential`
- `libncurses-dev`

If an argument is provided to the script, it is interpreted as the Git tag
to check out before compilation. Use it to specify a specific version to
compile. If no argument is provided, the latest commit is used.

```shell
./install-kakoune.sh
```

or to compile for a specific Git tag (change `v2020.09.01` as appropriate)

```shell
./install-kakoune.sh v2020.09.01
```

## Installing kakoune plugins

The plugin versions are specified in the shell script.

```shell
./install-plugins.sh
```

## Installing kak-lsp (LSP client)

If installing on MacOS, follow instructions above to symlink
`kak-lsp.toml` to the correct location.

The `kak-lsp` version is specified in the shell script.

```shell
./install-kak-lsp.sh
```

## repl-new and repl-send-text workaround for Kakoune version <= 2020.09.01

For Kakoune version `2020.09.01` or earlier, the aliases `repl-new` and
`repl-send-text` are named `repl` and `send-text` respectively. The provided
`kakrc` uses `repl-new` and `repl-send-text`, so they need to be defined. To
do this, create a local config file `$HOME/.config/kak/kakrc.local` with
the following contents.

```
hook global ModuleLoaded tmux-repl %{
    alias global repl-new tmux-repl-horizontal
    alias global repl-send-text tmux-send-text
}
hook global ModuleLoaded kitty-repl %{
    alias global repl-new kitty-repl
    alias global repl-send-text kitty-send-text
}
hook global ModuleLoaded x11-repl %{
    alias global repl-new x11-repl
    alias global repl-send-text x11-send-text
}
```
