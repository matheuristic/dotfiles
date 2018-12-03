# dotfiles

User configuration files for:

* Bash
* Emacs
* Flake8
* Git global gitignore files
* PostgreSQL aliases
* Pylint
* GNU Screen
* tmux
* Vim
* X11 (urxvt and xterm)

## Automated installation

To set up basic gitignore, NPM, tmux and Emacs config files for a new account:
```Shell
$ ./install.sh $PWD
```
from the root directory of the repository. The files will be set up as symlinks to corresponding files in the repository.

Optional Emacs UI elements and support for specific file formats and programming languages can be set up by copying the [post-init](.emacs.d/init-local-post.el) and modifying it as needed.

## Setting up base environment

If using [Anaconda](https://www.anaconda.com/download/) or [Miniconda](https://conda.io/miniconda.html), basic packages for Python and other development can be installed via:
```Shell
$ conda install --file base_conda_packages.txt
```
for a currently activated environment, or
```Shell
$ conda create --name <env> --file base_conda_packages.txt
```
if creating a new environment for Python development.

## Getting specific files

For \*nix machines:
```Shell
$ wget -q https://raw.githubusercontent.com/matheuristic/dotfiles/master/${filename}
```

For OS X machines:
```Shell
$ curl -LSs https://raw.githubusercontent.com/matheuristic/dotfiles/master/${filename}
```

Replace `${filename}` with the name of the file to be retrieved.
