# dotfiles

User configuration files for:

* Bash
* Emacs
* fish
* Flake8
* Git global gitignore files
* Neovim
* NPM
* PostgreSQL aliases
* Pylint
* GNU Screen
* tmux
* Vim
* X11 (urxvt and xterm)

## Project-specific pylint configuration

To specify a project-specific pylintrc file, create (or modify) a `pylintrc` file in the Python project root directory and add the following `init-hook` to the master section:
```
[MASTER]

init-hook="from pylint.config import find_pylintrc; import os, sys; sys.path.append(os.path.dirname(find_pylintrc()))"
```

You may also need to create (or modify) a `.dir-locals.el` file with the following contents in the project root directory so FlyCheck that uses the project-specific `pylintrc` file in Emacs:
```emacs
;; project-specific Emacs settings
((nil . ((flycheck-pylintrc . "pylintrc"))))
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
