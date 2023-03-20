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
}
