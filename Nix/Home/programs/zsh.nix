{ pkgs }:
let
  dotDir = ".config/zsh";
in {
  enable = true;
  enableCompletion = false;
  enableAutosuggestions = true;
  inherit dotDir;
  history = {
    size       = 50000;
    save       = 500000;
    path       = "${dotDir}/history";
    ignoreDups = true;
    share      = true;
    extended   = true;
  };
  initExtraBeforeCompInit = ''
    eval "$(/opt/homebrew/bin/brew shellenv)"
  '';
  plugins = [ ];
}
