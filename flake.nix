{
  description = "A Nix Home Manager Configuration for Paul Roden Henderson";

  inputs = {

    flake-utils = {
      url = "github:numtide/flake-utils/b1d9ab70662946ef0850d488da1c9019f3a9752a";
    };

    nixpkgs = {
      url = "github:NixOS/nixpkgs/d3651f9b43afac9d88e8e844324297439b7ac3e0";  
    };

    darwin = {
      url = "github:lnl7/nix-darwin/bcc8afd06e237df060c85bad6af7128e05fd61a3";  # master @ 2024-03-17
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/a7117efb3725e6197dd95424136f79147aa35e5b";  # master @ 2024-06-04
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fenix = {
      url = "github:nix-community/fenix/49bf3506637ee9f30ab589104f479ec61db5924c";  # master @ 2024-05-20
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake/1a53bd820143236c49118c8f1fa588c86ef2d43c";
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
      overlays = [
        emacs-lsp-booster.overlays.default
        fenix.overlays.default
        soft-serve.overlays.default
        (_: _: { eud = eudaemon.packages.${system}.default; })
        (import ./Nix/Home/programs/emacs/emacs-patch.nix { inherit pkgs; })
      ];
      config.allowUnfree = true;
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
