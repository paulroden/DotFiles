{ pkgs, config }:
let
  dotDir = ".config/zsh";
in {
  enable = true;
  enableCompletion = true;
  enableAutosuggestions = true;
  inherit dotDir;
  history = {
    size       = 50000;
    save       = 500000;
    path       = "${config.xdg.dataHome}/${dotDir}/history";
    ignoreDups = true;
    share      = true;
    extended   = true;
  };
  initExtraBeforeCompInit = ''
    eval "$(/opt/homebrew/bin/brew shellenv)"
  '';
  initExtra = ''
    export PATH="/Users/paul/.nix-profile/bin:/run/current-system/sw/bin/:/Users/paul/.cargo/bin:/Users/paul/.cabal/bin:/Users/paul/.ghcup/bin:/nix/var/nix/profiles/default/bin:/Users/paul/.orbstack/bin:/Users/paul/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/Developer/CommandLineTools/usr/bin:/Library/Apple/usr/bin"
  '';
  plugins = [ ];
}
