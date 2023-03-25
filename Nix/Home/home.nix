{ pkgs, system, lib, config, ... }:
let
  username = "paul";
  homeDirectory = "/Users/${username}";
  homebrewRoot = "/opt/homebrew";
in
{
  imports = [ ./dock-items.nix ];
  home = {
    stateVersion = "22.11";
    inherit username;
    inherit homeDirectory;
    packages = import ./packages.nix pkgs;
    sessionVariables = {
      # suppress Starship warnings in terminal (instead, see log: ~/.starship/cache)
      STARSHIP_LOG = "error";
      # Homebrew environment configuration (as per `brew shellenv')
      HOMEBREW_REPOSITORY = homebrewRoot;
      HOMEBREW_PREFIX = homebrewRoot;
      HOMEBREW_CELLAR = "${homebrewRoot}/Cellar";
      MANPATH = "${homebrewRoot}/share/man:\${MANPATH}";
      INFOPATH = "${homebrewRoot}/share/info:\${INFOPATH}";
      # emacs-vterm ref
      EMACS_VTERM_PATH = "${pkgs.emacs-vterm}";
      # hack to clear the PATH inherited from the system environment
      PATH = "";
    };
    # every item we want to have on the PATH should be declared below:
    sessionPath = [
      "${config.home.profileDirectory}/bin"
      "/run/current-system/sw/bin/"
      "${homeDirectory}/.cargo/bin"
#      "/opt/homebrew/opt/llvm@12/bin"
      "${homeDirectory}/.cabal/bin"
      "${homeDirectory}/.ghcup/bin"
      "/nix/var/nix/profiles/default/bin"
      "${homeDirectory}/bin"
      "${homebrewRoot}/bin"
      "${homebrewRoot}/sbin"
      "/usr/local/bin"
      "/usr/bin"
      "/bin"
      "/usr/sbin"
      "/sbin"
      # the below path is for the XCode command line tools and has caused issues
      # with build tools (e.g. `cargo`) and the linker `ld`: `/usr/bin/ld` is 
      # expected instead of the `ld` under this path. Priorise above `/usr/bin`.
      "/Library/Developer/CommandLineTools/usr/bin"
      "/Library/Apple/usr/bin"
    ];
    shellAliases = {
      ll = "exa -lag";
      lt = "exa -laT --level=2";
      ql = "qlmanage -p";  # quicklook -- MacOS only
      kks = "kitty +kitten ssh";
      # convenience function to the home-manager rebuild invocation, which
      # is a little fragile as it depends on the location of this very file
      home-rebuild = "home-manager switch --flake ~/DotFiles#${username}";
    };

    # GHCI config
    file.".ghci".text =
      '':set prompt "⋏ "
        :def pf \str -> return $ ":! pointfree \"" ++ str ++ "\""
      '';

    # Agda: state common libraries here for general availability
    file.".agda/defaults".text = ''
      standard-library
      cubical
    '';
    
    # kitticon: https://github.com/hristost/kitty-alternative-icon
    file.".config/kitty/kitty.app.icns".source = ./programs/kitty/kitty.app.icns;
  };
  programs = {
    home-manager.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    git = import ./programs/git.nix { inherit config; };
    fish = import ./programs/fish { inherit pkgs; };
    zsh = import ./programs/zsh.nix { inherit pkgs; };
    starship = import ./programs/starship.nix;
    kitty = import ./programs/kitty { inherit config; };
    bat = import ./programs/bat;
    vscode = import ./programs/vscode { inherit pkgs; };
  };
  local.dock = import ./programs/dock.nix { inherit config pkgs; };
}
