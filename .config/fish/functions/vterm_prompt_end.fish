# Helper function for redefining prompt in config.fish to enable directory
# and prompt tracking in emacs-libvterm
# https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
if [ "$INSIDE_EMACS" = "vterm" ]
    function vterm_prompt_end
        vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
    end
end