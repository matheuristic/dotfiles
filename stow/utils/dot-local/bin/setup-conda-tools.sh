#!/bin/sh

# setup-conda-tools.sh - create conda env from file and global command wrappers

# Create a conda env and install packages from a conda environment YAML file,
# and link a command for each installed package to $PREFIX/bin/ (one or more
# commands separated by whitespace in a comment after each package, or if
# none are indicated then a command of the same name as the package)

# Set the CONDA_ENV env var to change the conda cmd used (default: micromamba)

# Set the PREFIX env var to change prefix as mentioned above (default: ~/.local)

# Requirements: conda or variant (default: micromamba) installed and configured

# Example file that when "install"-ed with this script creates a conda-tools
# env with black, pandoc, python==3.11.8, and ripgrep packages installed and
# creates wrappers for black, pandoc, pandoc-lua, pandoc-server, and rg
# (python has an empty trailing comment so it is not wrapped; and tree is
# commented out so it is not installed):
#
#     name: conda-tools
#     channels:
#       - conda-forge
#     dependencies:
#       - black
#       - pandoc # pandoc pandoc-lua pandoc-server
#       - python=3.11.8 #
#       - ripgrep # rg
#       # - tree

set -e

CONDA_CMD=${CONDA_CMD:-micromamba}
PREFIX=${PREFIX:-$HOME/.local}

validate_conda_cmd () {
	if ! [ "$CONDA_CMD" == "conda" -o "$CONDA_CMD" == "mamba" -o "$(basename $CONDA_CMD)" == "micromamba" ]; then
		echo "Unsupported CONDA_CMD $CONDA_CMD" 1>&2
		exit 2
	fi
}

validate_file () {
	if [ -z "$1" ]; then
		echo 'No filename specified' 1>&2
		exit 2
	fi
	if [ ! -f "$1" ]; then
		echo "File $1 not found" 1>&2
		exit 2
	fi
}

get_envname () {
	# Assume no tab characters in file, and env name on the same line as 'name:'
	ENVNAME=$(cat "$1" | grep -E '^name:' | sed 's/^name: *//' | sed 's/ *$//')
	if [ -z "$ENVNAME" ]; then
		echo 'No environment name found' 1>&2
		exit 3
	fi
	echo "$ENVNAME"
}

get_dependencies_to_link () {
	# Assume no tab characters in file, and skip Python
	cat "$1" | sed -e '1,/^dependencies: *$/ d' | \
		sed -e '/^[^ ]/,$ d' | \
		grep -v '^ *#' | \
		sed 's/.*# *//g' | \
		sed 's/ *-  *//g' | \
		sed 's/[=<>].*//g' | \
		sed 's/ *$//g'
}

install_tools () {
	FILENAME="$1"
	validate_file "$FILENAME"
	ENVNAME=$(get_envname "$FILENAME")

	"$CONDA_CMD" env create -y -f "$FILENAME"

	for PROGNAME in $(get_dependencies_to_link "$FILENAME"); do
		if [ -z "$PROGNAME" ]; then
			continue
		fi
		DEST=$PREFIX/bin/$PROGNAME
	 	echo "Creating wrapper for $ENVNAME env $PROGNAME : $DEST"
		if [ "$(basename $CONDA_CMD)" == "micromamba" ]; then
			if [ ! -x "$MAMBA_ROOT_PREFIX/envs/$ENVNAME/bin/$PROGNAME" ]; then
				echo "WARNING: Skipping wrapper creation: environment $ENVNAME has no command $PROGNAME" 1>&2
			fi
			cat >"$DEST" <<EOF
#!/bin/sh

"$CONDA_CMD" run -n $ENVNAME $PROGNAME "\$@"
EOF
		elif [ "$CONDA_CMD" == "conda" -o "$CONDA_CMD" == "mamba" ]; then
			# Need to explicitly activate env when using
			# conda or mamba, else some wrapped commands
			# may not behave in the terminal as expected
			# (typically in cases where ncurses or piping
			# is being used). E.g., `bat somefile.sh`
			# acts like in a dumb term or
			# `cat test.md | prettier --parser=markdown`
			# may not output anything to stdout
			CONDA_SH=$(dirname $CONDA_EXE)
			if [ ! -x "$CONDA_SH/../envs/$ENVNAME/bin/$PROGNAME" ]; then
				echo "WARNING: Skipping wrapper creation: environment $ENVNAME has no command $PROGNAME" 1>&2
			fi
			cat >"$DEST" <<EOF
#!/bin/sh

. "$CONDA_SH/../etc/profile.d/conda.sh"
conda activate $ENVNAME
"$PROGNAME" "\$@"
EOF
		else
			echo "Unsupported command $CONDA_CMD" 1>&2
			exit 2
		fi
		chmod +x "$DEST"
	done

	echo "Done"
}

uninstall_tools () {
	FILENAME="$1"
	validate_file "$FILENAME"
	ENVNAME=$(get_envname "$FILENAME")

	for PROGNAME in $(get_dependencies_to_link "$FILENAME"); do
		if [ -z "$PROGNAME" ]; then
			continue
		fi
		DEST=$PREFIX/bin/$PROGNAME
		rm -f "$DEST"
		echo "Removed $DEST"
	done

	"$CONDA_CMD" env remove -y -n "$ENVNAME"

	echo "Done"
}

case $1 in
	install)
		validate_conda_cmd
		install_tools "$2"
		;;
	uninstall)
		validate_conda_cmd
		uninstall_tools "$2"
		;;
	*)
		echo "Usage: $(basename $0) <install|uninstall> <filename>" 1>&2
		exit 1
		;;
esac
