# Redefine clear function to also clear scrollback history in emacs-libvterm
# https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
if [ "$INSIDE_EMACS" = "vterm" ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    end
end