# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  home-manager =
    builtins.fetchTarball
      "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      (import "${home-manager}/nixos")
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nimo";
  # networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = {
  #   "eurosign:e";
  #   "caps:escape" # map caps to escape.
  # };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  users.users = {
    paul = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      shell = pkgs.fish;
      packages = with pkgs; [
        # emacs
      ];
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.paul = {
      home.stateVersion = "23.05";
      programs = {
        emacs = {
          enable = true;
        };
        fish = {
          enable = true;
          shellAliases = {
            ll = "exa -lag";
          };
        };
        home-manager = {
          enable = true;
        };
        starship = {
          enable = true;
          enableBashIntegration = true;
          enableFishIntegration = true;
          settings = {
            username = {
              format = "user: [$user]($style) ";
              show_always = true;
            };
            shlvl = {
              disabled = false;
              format = "$shlvl ▼ ";
              threshold = 4;
            };
          };
        };
      };
    };
  };

  environment.systemPackages = with pkgs; [
    bat
    broot
    dig
    du-dust
    exa
    fd
    file
    fish
    git
    helix
    home-manager
    inetutils
    ripgrep
    rsync
    starship
    vim
    wget
    zsh
    zsh-autocomplete
    zsh-completions
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Nix stuff
  nix.settings.trusted-users = [ "paul" "root" ];
  nix.distributedBuilds = true;  

  # Emulation & Cross-compiling
  boot.binfmt.emulatedSystems = [ "wasm32-wasi" "x86_64-linux" ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.settings.permitRootLogin = "yes";

  services.unbound = {
    enable = true;
    enableRootTrustAnchor = true;
    resolveLocalQueries = true;
    settings = {
      remote-control.control-enable = true;
      forward-zone = [
        {
          name = ".";
          forward-tls-upstream = "yes"; 
          forward-addr = [
            "2620:fe::fe           # Quad9"
            "2620:fe::9            # Quad9"
            "9.9.9.9@853           # Quad9"
            "2606:4700:4700::1111  # CloudFlare"
            "2606:4700:4700::1001  # CloudFlare"
            "2001:470:20::2        # Hurricane Electric"
            "74.82.42.42           # Hurricane Electric"
          ];
       }
    ];
    server = {
      interface = [
        "::"
        "0.0.0.0"
      ];
      access-control = [
        # allow localhost IP range to query Unbound's DNS
        "::1/128 allow"
        "127.0.0.0/8 allow"
        # also allow IPs on the local network
        "192.168.1.174 allow"
        "10.0.0.0/8 allow"
        "192.168.0.0/16 allow"
        ];
      };
    };
  };

  networking.dhcpcd.extraConfig = "nohook resolv.conf";
  networking.nameservers = [
    "::1"
    "127.0.0.1"
    #"192.168.1.174"
    #"1.1.1.1"
    #"9.9.9.9"
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It’s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}

