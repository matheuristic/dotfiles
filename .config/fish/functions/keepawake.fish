function keepawake --description "keepawake <hrs>"
    switch (uname)
        case Darwin
            # default caffeinate time of 8 hours
            set -u keepawakehours 8
            if test (count $argv) -ne 0
                set keepawakehours $argv[1]
            end
            caffeinate -i -t (math "$keepawakehours * 60 * 60")
        case '*'
            echo keepawake is only supported on Mac OS X
    end
end
