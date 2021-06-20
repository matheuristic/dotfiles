# https://www.jsoftware.com/
#

# Detection
# ‾‾‾‾‾‾‾‾‾

hook global BufCreate .*\.(ijs|ijt) %{
    set-option buffer filetype j
}

# Initialization
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾

hook global WinSetOption filetype=j %{
    require-module j

    add-highlighter window/j ref j
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/j }
}

hook global BufSetOption filetype=j %{
    set-option buffer comment_line 'NB.'
}

provide-module j %{

# Highlighters
# ‾‾‾‾‾‾‾‾‾‾‾‾

# Regions

add-highlighter shared/j         regions
add-highlighter shared/j/code    default-region group
add-highlighter shared/j/string  region  "'"     (?<!\\)(\\\\)*'   fill string
add-highlighter shared/j/comment region  'NB\.'  $                 fill comment

}
