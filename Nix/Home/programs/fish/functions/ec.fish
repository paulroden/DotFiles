# Function to launch emacsclient with default daemon in the current working
# directory, or by visting a file, if provided as an argument
function ec
    # Parse any of these optional flags:
    #   `--fg`:  optionally runs the emacs client as a _foreground_
    #            task (usually it's desirable to have it in the
    #            background to keep the shell session free).
    #   `debug`: echos the command used to launch `emacsclient`
    argparse 'fg=?' 'debug=?' -- $argv

    # Use initial arg. as file path, otherwise use current working
    # directory as starting point in emacs (in dired)
    if set -q argv[1]
        set file_path (realpath $argv[1])
    else
        set file_path $PWD
    end

    # Pass $ALTERNATE_EDITOR to emacsclient, if set, otherwise leave blank
    set -q ALTERNATE_EDITOR; or set ALTERNATE_EDITOR  "\"\"";

    # If $EMAC_SOCKET_NAME is set in the environment, use that, otherwise
    # use "default" as the Emacs daemon's socket name
    set -q EMACS_SOCKET_NAME; or set EMACS_SOCKET_NAME "default";

    # Compose a command to run as a background task is below.
    # note: the `--eval' statement ensures that emacs' init is loaded
    # this only happens once per session, so does not slow subsequent
    # client launches
    set cmd \
    "emacsclient \\
      --alternate-editor=$ALTERNATE_EDITOR \\
      --socket-name=$EMACS_SOCKET_NAME \\
      --create-frame \\
      --eval \"(progn (load-file \\\"~/.emacs.d/init.el\\\") (find-file \\\"$file_path\\\")) \" "

    # Print emacsclient invocation `cmd` if --debug flag is present
    if set -q _flag_debug
        echo "Invoking emacsclient as below; passing file path of '$file_path'"
        echo $cmd | bat -p --language=fish
    end
    
    # Start the daemon process here, in case its not already running.
    # `emacsdaemons` will only start the process its not already running.
    emacsdaemons --new $EMACS_SOCKET_NAME;

    if set -q _flag_fg
        # If `--fg` flag is set, run emacs client in the foreground (this is probably rare)
        fish -c $cmd
    else
        # Otherwise, run emacs client as a background task, ignoring `stdout` (but not `stderr`)
        fish -c $cmd 1>/dev/null &
    end

    # Everything went okay, provide a error-free return value
    return 0
end
