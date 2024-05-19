{ config, lib, pkgs, ... }:
{
  imports = [ ./Impure/homebrew.nix ];

  services = {
    nix-daemon = {
      enable = true;
    };

    emacs = {
      enable = true;
      package = pkgs.emacs';
    };

    skhd = {
      enable = true;
      package = pkgs.skhd;
    };    
  };

  programs = {
    zsh.enable = true;
  };

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

  security = {
    pam.enableSudoTouchIdAuth = true;
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
      # Cloudflare
      "2606:4700:4700::1111"
      "2606:4700:4700::1001"
      "1.1.1.1"
      "1.0.0.1"
    ];
  };
  
  fonts = {
    fontDir.enable = true;
    fonts = [
      pkgs.cardo
      pkgs.charis-sil
      pkgs.crimson-pro
      pkgs.fira
      pkgs.font-awesome
      pkgs.gyre-fonts
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
      # pkgs.noto-fonts
      # pkgs.noto-fonts-cjk
      # pkgs.noto-fonts-emoji
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

  # MacOS defaults - full list here: https://daiderd.com/nix-darwin/manual/index.html
  system = {
    defaults = {
      dock = {
        autohide = true;
        orientation = "left";
        autohide-time-modifier = 0.3;
      };
    };
  };
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
