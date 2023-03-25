# Function to list any and all emacs daemon processes currently running
# If the emacs server (daemon) is not running, it returns an error to stderr
# note: use of `ls -p` is applicable to BSD-like systems (e.g. MacOS) to
#       remove appended "=" indicating that these files are sockets
function emacsdaemons
    argparse 'new=?' 'kill=?' -- $argv

    set SOCKET_NAME $argv[1]
    
    if set -q _flag_kill
        if not set -q SOCKET_NAME
            echo "Error: no socket name provided.\n   Usage: `emacsdaemons --kill SOCKET_NAME`" 1>&2
            return 1
        end
        if not contains $SOCKET_NAME (extant_emacs_daemons)
            echo "Socket name '$SOCKET_NAME' does not exist."
        else
            set DAEMON_PID (get_emacs_daemon_pid $SOCKET_NAME)
            fish -c "kill $DAEMON_PID; sleep 1"; and wait $DAEMON_PID 2>/dev/null
            echo "Ended Emacs daemon process ($DAEMON_PID) with socket name '$SOCKET_NAME'."
        end
        
        echo "Existing Emacs daemon socket names:"
        for name in (extant_emacs_daemons)
            echo $name
        end
        return 0
    end

    
    # if '--new' flag is passed in, create a new daemon process named by $argv[1]
    if set -q _flag_new
        if not set -q SOCKET_NAME
            echo "Error: no socket name provided.\n    Usage: `emacsdaemons --new SOCKET_NAME`" 1>&2
            return 1
        end
        if contains $SOCKET_NAME (extant_emacs_daemons)
            echo "Emacs daemon '$SOCKET_NAME' is running."
        else
            echo "Starting Emacs daemon '$SOCKET_NAME'..."
            # set EMACS_BIN (dirname (readlink (which emacs)))/../Applications/Emacs.app/Contents/MacOS/Emacs   # see note at extant_emacs_daemons
	    emacs --daemon=$SOCKET_NAME
        end
    # no flags are passed; just list the currently existing daemon names
    else
        for name in (extant_emacs_daemons)
            echo $name
        end
    end
    return 0
end

function get_emacs_daemon_pid
    ps aux | rg -S emacs | rg "daemon=\\\0123,4\\\012$argv[1]" | awk '{print $2}'
end

# Print any current running Emacs daemon sockets
# (These can be found as files in the Emacs user temp' directory:
#   /tmp/emacs{user-id}/ if invoked with `emacs`; $TMPDIR instead of /tmp if invoked with
#   .../Applications/Emacs.app/Contents/MacOS/Emacs, for some reason).
# If Emacs is not running, the directory may not exist, so return nothing & no error.
function extant_emacs_daemons
    ls -p /tmp/emacs(id -u) 2>/dev/null 
    return 0
end

