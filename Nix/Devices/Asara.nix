{ config, pkgs, nixpkgs, ... }:
{
  services.nix-daemon.enable = true;
  
  programs.zsh.enable = true;

  nix = {
    settings = {
      trusted-users = [ "paul" "root" ];
    };
#    configureBuildUsers = true;
#    nrBuildUsers = 32;
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
      pkgs.fira-code
      pkgs.fira-code-symbols
      pkgs.hasklig
      pkgs.ibm-plex
      pkgs.iosevka-comfy.comfy
      pkgs.iosevka-comfy.comfy-fixed
      pkgs.iosevka-comfy.comfy-duo
      pkgs.jetbrains-mono
      pkgs.julia-mono
      pkgs.liberation_ttf
      pkgs.merriweather
      pkgs.merriweather-sans
      pkgs.mononoki
      pkgs.mplus-outline-fonts.githubRelease
      pkgs.noto-fonts
      pkgs.noto-fonts-cjk
      pkgs.noto-fonts-emoji
      pkgs.open-sans
      pkgs.sarasa-gothic
      pkgs.source-code-pro
      pkgs.source-han-mono
      pkgs.weather-icons
    ];
  };
  
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
