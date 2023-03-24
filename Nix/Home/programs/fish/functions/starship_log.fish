function starship_log
    set -q STARSHIP_CACHE; or set STARSHIP_CACHE ~/.starship/cache
    set session_log_file "$STARSHIP_CACHE/session_$STARSHIP_SESSION_KEY.log"
    if test -e $session_log_file
        bat $session_log_file
    else
        echo "No Starship Log for current session ($STARSHIP_SESSION_KEY)";
    end
end
