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

```shell
./install-kakoune.sh
```

## Installing kak-lsp (LSP client)

If installing on MacOS, follow instructions above to symlink
`kak-lsp.toml` to the correct location.

```shell
./install-kak-lsp.sh
```
