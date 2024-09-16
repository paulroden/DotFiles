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
    packages = with pkgs; [
      cardo
      charis-sil
      crimson-pro
      fira
      font-awesome
      gyre-fonts
      hasklig
      ibm-plex
      inriafonts
      iosevka-comfy.comfy
      iosevka-comfy.comfy-fixed
      iosevka-comfy.comfy-duo
      jetbrains-mono
      julia-mono
      kanji-stroke-order-font
      liberation_ttf
      maple-mono
      merriweather
      merriweather-sans
      mononoki
      mplus-outline-fonts.githubRelease
      nacelle
      # noto-fonts
      # noto-fonts-cjk
      # noto-fonts-emoji
      open-sans
      sarasa-gothic
      source-code-pro
      source-han-mono
      source-han-sans
      source-han-serif
      source-sans
      sorts-mill-goudy
      stix-two
      weather-icons
      (nerdfonts.override {
        fonts = [
          "FiraCode"
          "Hasklig"
        ];
      })
    ];
  };

  # MacOS defaults
  #  - full list here: https://daiderd.com/nix-darwin/manual/index.html
  #  - see also: https://macos-defaults.com
  system = {
    defaults = {
      dock = {
        autohide = true;
        orientation = "left";
        autohide-time-modifier = 0.3;
        expose-group-by-app = false;  # TODO: looks like this should be expose-group-by-app instead - PR?
        "expose-group-apps" = false;
      };
    };
  };
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
