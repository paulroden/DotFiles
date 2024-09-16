{
  description = "A Nix Home Manager Configuration for Paul Roden Henderson";

  inputs = {

    flake-utils = {
      url = "github:numtide/flake-utils/b1d9ab70662946ef0850d488da1c9019f3a9752a";
    };

    nixpkgs = {
      url = "github:NixOS/nixpkgs/c20c714d7cdf582c9c9db6bdac37c407d33d1789";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/f7142b8024d6b70c66fd646e1d099d3aa5bfec49";  # master @ 2024-08-03
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/afc892db74d65042031a093adb6010c4c3378422";  # master @ 2024-08-02
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    fenix = {
      url = "github:nix-community/fenix/69c2c0c3c2f56314966dae21d79274515b228482";  # main @ 2024-08-03
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake/7d110295988fc9bf7fd43bb0cabfbe58a4a5ecf8";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    soft-serve = {
      url = "github:paulroden/soft-serve";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    eudaemon = {
      url = "github:paulroden/eud";
    };

  };

  outputs =
    { nixpkgs
    , darwin
    , home-manager
    , emacs-lsp-booster
    , fenix
    , eudaemon
    , soft-serve
    , ...
    }:
  let
    system = "aarch64-darwin";
    pkgs = import nixpkgs {
      localSystem = system;
      config.allowUnfree = true;
      overlays = [
        emacs-lsp-booster.overlays.default
        fenix.overlays.default
        soft-serve.overlays.default
        (_: _: { eud = eudaemon.packages.${system}.default; })
        (import ./Nix/Home/programs/emacs/emacs-patch.nix { inherit pkgs; })
      ];
    };
  in {
    packages.${system}.default = fenix.packages.${system}.minimal.toolchain;
    darwinConfigurations = {
      "Asara" = darwin.lib.darwinSystem {
        inherit pkgs system;
        inputs = { inherit darwin home-manager nixpkgs pkgs; };
        modules = [ ./Nix/Devices/Asara ];
      };
    };
    homeConfigurations = {
      "paul" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./Nix/Home/home.nix ];
      };
    };
  };
}
