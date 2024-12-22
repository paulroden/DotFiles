{
  description = "A Nix Home Manager Configuration for Paul Roden Henderson";

  inputs = {

    flake-utils = {
      url = "github:numtide/flake-utils/11707dc2f618dd54ca8739b309ec4fc024de578b";  # master @ 2024-11-13
    };

    nixpkgs = {
      url = "github:NixOS/nixpkgs/3ab7056874d66b3e20693c871c7340dd5d81cea3";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/a35b08d09efda83625bef267eb24347b446c80b8";  # master @ 2024-12-07
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/c903b1f6fbfc2039963806df896f698331b77aa8";  # master @ 2024-12-21
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    fenix = {
      url = "github:nix-community/fenix/6c2c568c407a0489295194900be09e75c6603a25";  # main @ 2024-12-22
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
    , fenix
    , eudaemon
    , ...
    }:
  let
    system = "aarch64-darwin";
    pkgs = import nixpkgs {
      localSystem = system;
      config.allowUnfree = true;
      overlays = [
        fenix.overlays.default
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
