# dotfiles

User configuration files for:

- [Bash](https://www.gnu.org/software/bash/)
- [Elixir](https://elixir-lang.org/)
- [fish](https://fishshell.com/)
- [Flake8](https://flake8.pycqa.org/en/latest/)
- [Git](https://git-scm.com/)
- [kitty](https://sw.kovidgoyal.net/kitty/)
- [Neovim](https://neovim.io/)
- [NPM](https://www.npmjs.com/) using [nvm](https://github.com/nvm-sh/nvm)
- [PostgreSQL](https://www.postgresql.org/)
- [Proselint](https://github.com/amperser/proselint)
- [Python](https://www.python.org/) using
  [Pylint](https://www.pylint.org/) for linting
- [R](https://www.r-project.org/) (for installations via `conda` on macOS)
- [Ranger](https://github.com/ranger/ranger)
- [GNU Screen](https://www.gnu.org/software/screen/)
- [tmux](https://github.com/tmux/tmux)
- [Vi](https://en.wikipedia.org/wiki/Vi) (applicable to
  [nvi](https://repo.or.cz/nvi.git),
  [nvi2](https://github.com/lichray/nvi2) and
  [openvi](https://github.com/johnsonjh/OpenVi))
- [Vim](https://www.vim.org/)
- [Vis](https://github.com/martanne/vis)
- [xxdiff](https://github.com/blais/xxdiff)
- [X11](https://www.x.org/wiki/) ([urxvt](http://software.schmorp.de/pkg/rxvt-unicode.html) and [xterm](https://invisible-island.net/xterm/))
- [Zsh](https://www.zsh.org/)
- ... and other utility scripts

For Acme configuration, see [here](https://github.com/plan9port-config).

For Emacs configuration, see [here](https://github.com/matheuristic/emacs-config).

For Org example documents, see [here](https://github.com/matheuristic/org-examples).

## Deploying config files using GNU Stow

Install [GNU Stow](https://www.gnu.org/software/stow/) (on a
Debian-derived distribution use `sudo apt install stow`; on macOS use
`sudo port install stow` if MacPorts is installed).

Git clone the repository, and symlink each package's config files to
the user home directory using `stow`. A package name simply
corresponds to a directory within the repository, and `stow` with the
`--no-folding` option will create the directory structure within the
package at the target destination if it does not exist and
symbolically link each file in the package to the corresponding
location in the target destination, creating any required directories
as needed. Two examples (tmux and fish) are shown as follows.

```Shell
$ git clone https://github.com/matheuristic/dotfiles.git
$ cd dotfiles
$ stow -t $HOME --no-folding tmux
$ stow -t $HOME --no-folding fish
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
