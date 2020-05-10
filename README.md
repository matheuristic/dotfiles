# dotfiles

User configuration files for:

* [Bash](https://www.gnu.org/software/bash/)
* [fish](https://fishshell.com/)
* [Flake8](https://flake8.pycqa.org/en/latest/)
* [Git](https://git-scm.com/) global [gitignore](https://git-scm.com/docs/gitignore) files
* [kitty](https://sw.kovidgoyal.net/kitty/)
* [Neovim](https://neovim.io/)
* [NPM](https://www.npmjs.com/)
* [PostgreSQL](https://www.postgresql.org/) aliases
* [Pylint](https://www.pylint.org/)
* [GNU Screen](https://www.gnu.org/software/screen/)
* [tmux](https://github.com/tmux/tmux)
* [R](https://www.r-project.org/) (for installations via `conda` on Mac OS X)
* [Ranger](https://github.com/ranger/ranger)
* [Vim](https://www.vim.org/)
* [X11](https://www.x.org/wiki/) ([urxvt](http://software.schmorp.de/pkg/rxvt-unicode.html) and [xterm](https://invisible-island.net/xterm/))

For Emacs configuration, see [here](https://github.com/matheuristic/emacs-config).

For Org example documents, see [here](https://github.com/matheuristic/org-examples).

## Getting specific config files directly from Github

For \*nix machines:
```Shell
$ wget -q https://raw.githubusercontent.com/matheuristic/dotfiles/master/${filename}
```

For OS X machines:
```Shell
$ curl -LSs https://raw.githubusercontent.com/matheuristic/dotfiles/master/${filename}
```

Replace `${filename}` with the name of the file to be retrieved.
