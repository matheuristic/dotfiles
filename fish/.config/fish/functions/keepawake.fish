function keepawake --description "keepawake <hrs> (default: 8)"
    switch (uname)
        case Darwin
            # default caffeinate time of 8 hours
            set -u keepawakehours 8
            if test (count $argv) -ne 0
                set keepawakehours $argv[1]
            end
            caffeinate -i -t (math "$keepawakehours * 60 * 60")
        case '*'
            echo "keepawake is Mac OS X only. It wraps the caffeinate cmd"
    end
end
