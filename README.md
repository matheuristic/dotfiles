# dotfiles

- [Font notes](fonts.md)
- [Software notes](software.md)
- [User config files and tool scripts](stow), `stow`-deployable
  - [`bash`](stow/bash):
    [Bash](https://www.gnu.org/software/bash/) config
  - [`elixir`](stow/elixir):
    [Elixir](https://elixir-lang.org/) interactive shell config
  - [`git`](stow/git):
    [Git](https://git-scm.com/)
  - [`nvm`](stow/nvm):
    [nvm](https://github.com/nvm-sh/nvm) scripts
  - [`pandoc`](stow/pandoc):
    [Pandoc](https://pandoc.org/) scripts
  - [`psql`](stow/psql):
    [PostgreSQL](https://www.postgresql.org/) `psql` config
  - [`python`](stow/python):
    [Python](https://www.python.org/)
    [Flake8](https://flake8.pycqa.org/en/latest/) (linter),
    [Pylint](https://www.pylint.org/) (linter), and
    [pdb](https://docs.python.org/3/library/pdb.html) configs
  - [`screen`](stow/screen):
    [GNU Screen](https://www.gnu.org/software/screen/) config
  - [`tmux`](stow/tmux):
    [tmux](https://github.com/tmux/tmux) config
  - [`utils`](stow/utils):
    Other utility scripts
  - [`vi`](stow/vi):
    [Vi](https://en.wikipedia.org/wiki/Vi) config for
    [nvi](https://repo.or.cz/nvi.git),
    [nvi2](https://github.com/lichray/nvi2) and
    [openvi](https://github.com/johnsonjh/OpenVi)
  - [`vim`](stow/vim):
    [Vim](https://www.vim.org/) config
  - [`vis`](stow/vis):
    [Vis](https://github.com/martanne/vis) config
  - [`xxdiff`](stow/xxdiff):
    [xxdiff](https://github.com/blais/xxdiff) config
  - [`zsh`](stow/zsh):
    [Zsh](https://www.zsh.org/) config
- Other configuration and notes
  - [Acme](https://github.com/matheuristic/plan9port-config)
  - [Emacs](https://github.com/matheuristic/emacs-config)
    (or a simpler [alternative](https://emacs.amodernist.com/))
    - [Org example docs](https://github.com/matheuristic/org-examples)

## Deploying user config files and tool scripts using GNU Stow

Install [GNU Stow](https://www.gnu.org/software/stow/) (on a
Debian-derived distribution use `sudo apt install stow` ; on macOS use
`sudo port install stow` if MacPorts is installed).

Git clone the repository, and symlink each package's config files to
the user home directory using `stow`. Each repository subdirectory in
`stow/` (e.g.`stow/package`) corresponds to a package of the same
name, and `stow` with the `--no-folding` and `--dotfiles` options will
create the directory structure within the package at the target
destination if it does not exist and symbolically link each file in
the package to the corresponding location in the target destination,
creating any required directories as needed and changing `dot-XYZ`
names to `.XYZ`. Two examples (tmux and fish) are shown as follows.

```Shell
$ git clone https://github.com/matheuristic/dotfiles.git
$ cd dotfiles
$ stow -t $HOME --dotfiles --no-folding stow/tmux
$ stow -t $HOME --dotfiles --no-folding stow/fish
...
```

**References**:

- [Using GNU Stow to manage your dotfiles](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html)

## Shell types

There are different shell types, discussed using Bash since that is
the default shell for most distributions. A shell can be of multiple
types, for instance a login interactive shell.

- Login shell. A login Bash shell reads `~/.bash_profile` (or
  `~/.profile` if `~/bash_profile` does not exist) on start and
  `~/.bash_logout` on exit.
- Non-interactive shell. A non-interactive Bash shell reads the file
  specified by `$BASH_ENV` on start.
- Interactive shell. A interactive Bash shell reads ~/.bashrc on
  start.

Note that while Bash is the default shell on most Linux systems,
Zsh is the default shell on macOS systems.
