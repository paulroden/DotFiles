{ pkgs, config, lib, ... }:
let
  username = "paul";
  homeDirectory = "/Users/${username}";
  homebrewRoot = "/opt/homebrew";
  frameworks = pkgs.darwin.apple_sdk.frameworks;
  cFlags = lib.concatStringsSep " " [
    "-L${pkgs.libiconv}/include"
    "-L${homebrewRoot}/include"
    "-F${frameworks.CoreFoundation}/Library/Frameworks"
    "-F${frameworks.Security}/Library/Frameworks"
    "$CFLAGS"
  ];
  linkerFlags = lib.concatStringsSep " " [
    "-L${pkgs.libiconv}/lib"
    "-L${homebrewRoot}/lib"
    "-F${frameworks.CoreFoundation}/Library/Frameworks"
    "-framework CoreFoundation"
     "-F${frameworks.Security}/Library/Frameworks"
    "-framework Security"
    "$LDFLAGS"
  ];
  libraryPath = "$LIBRARY_PATH:${pkgs.libiconv}/lib:${homebrewRoot}/lib:/usr/lib";
  cargoPath =  "${config.xdg.dataHome}/cargo";
in
{
  imports = [ ./dock-items.nix ];
  xdg = {
    enable = true;
  };
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
      EMACS_EXEC = "${pkgs.emacs'}/Applications/Emacs.app/Contents/MacOS/Emacs";
      EMACS_VTERM_PATH = "${pkgs.emacsPackages.vterm}";
      EMACS_PDF_TOOLS = "${pkgs.emacsPackages.pdf-tools}";
      LIBRARY_PATH = libraryPath;
      CFLAGS = cFlags;
      CPPFLAGS = cFlags;
      LDFLAGS = linkerFlags;
      NIX_LDFLAGS = lib.replaceStrings ["LDFLAGS"] ["NIX_LDFLAGS"] linkerFlags;
      RUSTFLAGS = "-L framework=${frameworks.CoreFoundation}/Library/Frameworks -l framework=CoreFoundation -L framework=${frameworks.Security}/Library/Frameworks -l framework=Security";
      CARGO_HOME = "${cargoPath}";
      # hack to clear the PATH inherited from the system environment
      PATH = "";
    };
    # every item we want to have on the PATH should be declared below:
    sessionPath = [
      "${config.home.profileDirectory}/bin"
      "/run/current-system/sw/bin/"
      "${cargoPath}/bin"
      "${homeDirectory}/.cabal/bin"
      "${homeDirectory}/.ghcup/bin"
      "/nix/var/nix/profiles/default/bin"
      "${homeDirectory}/.orbstack/bin"
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
      ll = "eza -lag";
      lt = "eza -laT --level=2";
      ql = "qlmanage -p";  # quicklook -- MacOS only
      sk = "kitten ssh";
      emacs = "${pkgs.emacs'}/Applications/Emacs.app/Contents/MacOS/Emacs";
      neofetch = "${pkgs.fastfetch}/bin/fastfetch";
    };

    # GHCI config
    file.".ghci".text =''
        :set prompt "⋏ "
        :def pf \str -> return $ ":! pointfree \"" ++ str ++ "\""
      '';

    # Agda: state common libraries here for general availability
    file.".agda/defaults".text = ''
      standard-library
      cubical
    '';

    # kitticon: https://github.com/hristost/kitty-alternative-icon
    file.".config/kitty/kitty.app.icns".source = ./programs/kitty/kitty.app.icns;
    file.".config/kitty/search.py".source = ./programs/kitty/search.py;
    file.".config/kitty/scroll_mark.py".source = ./programs/kitty/scroll_mark.py;

    # skhd config
    file.".config/skhd/skhdrc".source = ./programs/skhd/skhdrc;

    # custom scripts to live in ~/bin
    file."bin/new-safari-window" = {
      source = ./programs/scripts/new-safari-window;
      executable = true;
    };

  };
  programs = {
    home-manager.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableBashIntegration = true;
    };
    man = {
      enable = true;
      generateCaches = false;
    };
    git = import ./programs/git.nix { inherit config; };
    fish = import ./programs/fish { inherit pkgs; };
    zsh = import ./programs/zsh.nix { inherit pkgs config; };
    nushell = import ./programs/nu { inherit pkgs config; };
    starship = import ./programs/starship.nix;
    kitty = import ./programs/kitty { inherit config; };
    bat = import ./programs/bat;
    vscode = import ./programs/vscode { inherit pkgs; };
  };

  local.dock = import ./programs/dock { inherit config pkgs; };

  targets.darwin.defaults = {
    "com.apple.dock" = {
      tilesize = lib.mkDefault 40;
      size-immutable = lib.mkDefault true;
      expose-group-apps = lib.mkDefault true;
    };
    "com.manytricks.Moom" = import ./programs/moom { inherit lib; };
  };
}
