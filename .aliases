# ~/.alias -*- conf-unix -*-
# GNU Linux userland
if ls --color > /dev/null 2>&1; then # GNU `ls`
  LS_COLOR_FLAG="--color"
else # macOS/BSD `ls`
  LS_COLOR_FLAG="-G"
fi
alias rm="rm -i" # Ask before removing a file
alias ls="ls -hF ${LS_COLOR_FLAG}" # Colorized ls
alias ll="ls -lhF --group-directories-first ${LS_COLOR_FLAG}"
alias lsd="ls -lhF ${LS_COLOR_FLAG} | grep --color=never '^d'"
# Debian package management
alias deblast='grep " install " /var/log/dpkg.log | tail -n 20'
# RPM package management
alias rpmlast='rpm -qa --last'
# Python package management
alias pip2updateuserpackages='pip2 freeze --user | grep -v "^\-e" | cut -d = -f 1 | xargs -n1 pip2 install --user --upgrade'
alias pip3updateuserpackages='pip3 freeze --user | grep -v "^\-e" | cut -d = -f 1 | xargs -n1 pip3 install --user --upgrade'
# Lynx
alias lynx='lynx -cookies'
# Hashing
command -v md5sum > /dev/null || alias md5sum="md5"  # alias `md5` to `md5sum` in macOS
command -v sha1sum > /dev/null || alias sha1sum="shasum"  # alias `shasum` to `sha1sum` in macOS
# Intuitive map function.  E.g. list directories containing some file:
# $ find . -name .gitattributes | map dirname
alias map="xargs -n1"
