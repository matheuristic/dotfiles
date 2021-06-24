# sh filetype configuration

hook global BufSetOption filetype=sh %{
    # Lint using shellcheck
    evaluate-commands %sh{
        if command -v shellcheck > /dev/null 2>&1; then
            printf "set buffer lintcmd 'shellcheck -f gcc -x -a'"
        else
            echo "echo -debug 'shellcheck not installed'"
        fi
    }
}
