# Bash login shell settings
# Updated: Mar 03 2016

# NOTE: There are 3 types of bash shells
# - the login shell (reads ~/.bash_profile on start and ~/.bash_logout on exit)
# - the non-interactive shell (reads the file specified by $BASH_ENV on start)
# - the interactive shell (reads ~/.bashrc on start)

# Load user bashrc
test -s ~/.bashrc && source ~/.bashrc
