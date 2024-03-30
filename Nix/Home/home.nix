{ pkgs, config, lib, ... }:
let
  username = "paul";
  homeDirectory = "/Users/${username}";
  homebrewRoot = "/opt/homebrew";
  frameworks = pkgs.darwin.apple_sdk.frameworks;
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
      # emacs-vterm ref
      EMACS_VTERM_PATH = "${pkgs.emacs-vterm}";
      # linking libiconv from clang seems to be a pervasive issue on MacOS with aarch...
      LIBRARY_PATH="$LIBRARY_PATH:${pkgs.libiconv}/lib";
      # linker stuff - it never ends...
      CFLAGS="-L${pkgs.libiconv}/include -F${frameworks.CoreFoundation}/Library/Frameworks $CFLAGS";
      CPPFLAGS="-L${pkgs.libiconv}/include -F${frameworks.CoreFoundation}/Library/Frameworks $CPPFLAGS";
      LDFLAGS = "-L${pkgs.libiconv}/lib -F${frameworks.CoreFoundation}/Library/Frameworks -framework CoreFoundation $LDFLAGS";
      LDCONFIG = "-F${frameworks.CoreFoundation}/Library/Frameworks";
      NIX_LDFLAGS = "-L${pkgs.libiconv}/lib -F${frameworks.CoreFoundation}/Library/Frameworks -framework CoreFoundation $NIX_LDFLAGS";
      # this effing works 🦀🦀🦀 !!
      RUSTFLAGS = "-L framework=${frameworks.CoreFoundation}/Library/Frameworks -l framework=CoreFoundation";

      # hack to clear the PATH inherited from the system environment
      PATH = "";
    };
    # every item we want to have on the PATH should be declared below:
    sessionPath = [
      "${config.home.profileDirectory}/bin"
      "/run/current-system/sw/bin/"
      "${homeDirectory}/.cargo/bin"
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
      sk = "kitten ssh";
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

    # skhd config
    file.".config/skhd/skhdrc".source = ./programs/skhd/skhdrc;
  };
  programs = {
    home-manager.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableBashIntegration = true;
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
  
  targets.darwin.defaults = {
    "com.apple.dock" = {
      tilesize = lib.mkDefault 40;
      size-immutable = lib.mkDefault true;
      expose-group-apps = lib.mkDefault true;
    };
    "com.manytricks.Moom" = import ./programs/moom { inherit lib; };
  };
}
