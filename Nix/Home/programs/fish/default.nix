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
    ec = builtins.readFile ./functions/ec.fish;
    emacsdaemons = builtins.readFile ./functions/emacsdaemons.fish;
    starship_log = builtins.readFile ./functions/starship_log.fish;
  };
}