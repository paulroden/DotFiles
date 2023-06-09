{ config, lib, nixpkgs, pkgs,  ... }:
{
  imports = [ ./Impure/homebrew.nix ];
  
  services = {
    nix-daemon.enable = true;

    emacs = {
      enable = true;
      package = pkgs.emacsGit;
    };
  };
  
  programs.zsh.enable = true;

  nix = {
    settings = {
      trusted-users = [ "paul" "root" ];
    };
    package = pkgs.nixFlakes;
    extraOptions =
      lib.optionalString (config.nix.package == pkgs.nixFlakes)
        "experimental-features = nix-command flakes";
    configureBuildUsers = true;
    nrBuildUsers = 32;
    distributedBuilds = true;
    buildMachines = [
      {
        hostName = "nemo";
        systems = [
          "aarch64-linux"
          "x86_64-linux"
          "wasm32-wasi"
        ];
        sshKey = "/var/root/.ssh/id_ed25519";
        maxJobs = 8;
        speedFactor = 2;
      }
    ];
  };

  environment = {
    darwinConfig = "$HOME/Dotfiles/Nix/Devices/Asara.nix";
    systemPackages = [
      pkgs.nix-deploy
      pkgs.utm
    ];
  };

  networking = {
    knownNetworkServices = [
      "Wi-Fi"
    ];
    dns = [
      # Quad9
      "2620:fe::fe"
      "2620:fe::9"
      "9.9.9.9"
      "149.112.112.112"
    ];
  };
  
  fonts = {
    fontDir.enable = true;
    fonts = [
      pkgs.cardo
      pkgs.charis-sil
      pkgs.crimson-pro
      pkgs.fira
      pkgs.fira-code
      pkgs.fira-code-symbols
      pkgs.font-awesome
      pkgs.gyre-fonts
      pkgs.hasklig
      pkgs.ibm-plex
      pkgs.inriafonts
      pkgs.iosevka-comfy.comfy
      pkgs.iosevka-comfy.comfy-fixed
      pkgs.iosevka-comfy.comfy-duo
      pkgs.jetbrains-mono
      pkgs.julia-mono
      pkgs.kanji-stroke-order-font
      pkgs.liberation_ttf
      pkgs.maple-mono
      pkgs.merriweather
      pkgs.merriweather-sans
      pkgs.mononoki
      pkgs.mplus-outline-fonts.githubRelease
      pkgs.nacelle
      pkgs.noto-fonts
      pkgs.noto-fonts-cjk
      pkgs.noto-fonts-emoji
      pkgs.open-sans
      pkgs.sarasa-gothic
      pkgs.source-code-pro
      pkgs.source-han-mono
      pkgs.source-han-sans
      pkgs.source-han-serif
      pkgs.source-sans 
      pkgs.sorts-mill-goudy
      pkgs.stix-two
      pkgs.weather-icons
      (pkgs.nerdfonts.override {
        fonts = [
          "FiraCode"
          "Hasklig"
        ];
      })
    ];
  };
  
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
