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
    [
      "done"
      "foreign-env"
      "puffer"
      # "sponge"
    ];
  functions = builtins.foldl'
    (acc: x: acc // { "${x}".body = builtins.readFile ./functions/${x}.fish; })
    {}
    [
      "ec"
      "emacsdaemons"
      "iplot"
      "starship_log"
    ];
}
