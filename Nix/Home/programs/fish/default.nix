{ pkgs }:
{
  enable = true;
  interactiveShellInit = ''
        set fish_greeting
        nix-your-shell fish | source
      '';
  plugins = map
    (plugin:
      { name = plugin;
        src  = pkgs.fishPlugins.${plugin}.src;
      })
    [ "done"
      "foreign-env"
      "puffer"
      "sponge"
    ];
  functions = {
    ec.body = builtins.readFile ./functions/ec.fish;
    emacsdaemons.body = builtins.readFile ./functions/emacsdaemons.fish;
    starship_log = {
      body = ''
        set -q STARSHIP_CACHE; or set STARSHIP_CACHE ~/.starship/cache
        set session_log_file "$STARSHIP_CACHE/session_$STARSHIP_SESSION_KEY.log"
        if test -e $session_log_file
           ${pkgs.bat}/bin/bat $session_log_file
        else
          echo "No Starship Log for current session ($STARSHIP_SESSION_KEY)";
        end
      '';
    };
  };
}
