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

## Setting up conda environment

If using [Anaconda](https://www.anaconda.com/download/) or [Miniconda](https://conda.io/miniconda.html), basic packages for Python development can be set up via:
```Shell
$ conda install --file base_conda_packages.txt
```
to install the packages into a currently activated environment, or
```Shell
$ conda create --name <env> --file base_conda_packages.txt
```
to create a new environment and install the packages into it.

## Project-specific pylint configuration

To specify a project-specific pylintrc file, create a pylintrc file at the Python project root directory and add the appropriate `init-hook` to the master section:
```
[MASTER]

init-hook="from pylint.config import find_pylintrc; import os, sys; sys.path.append(os.path.dirname(find_pylintrc()))"
```

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
