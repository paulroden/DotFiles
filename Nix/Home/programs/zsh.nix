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
  plugins = [
    {
      name = "iterm2_shell_integration";
      src = pkgs.fetchurl {
        url = "https://iterm2.com/shell_integration/zsh";
        sha256 = "1xk6kx5kdn5wbqgx2f63vnafhkynlxnlshxrapkwkd9zf2531bqa";
        # date = 2022-12-28T10:15:23-0800;
      };
    }
    {
      name = "iterm2_tmux_integration";
      src = pkgs.fetchurl {
        url = "https://gist.githubusercontent.com/antifuchs/c8eca4bcb9d09a7bbbcd/raw/3ebfecdad7eece7c537a3cd4fa0510f25d02611b/iterm2_zsh_init.zsh";
        sha256 = "1v1b6yz0lihxbbg26nvz85c1hngapiv7zmk4mdl5jp0fsj6c9s8c";
        # date = 2022-12-28T10:15:27-0800;
      };
    }
  ];
}
