{
  description = "A Nix Home Manager Configuration for Paul Roden Henderson";

  inputs = {

    flake-utils = {
      url = "github:numtide/flake-utils/b1d9ab70662946ef0850d488da1c9019f3a9752a";
    };

    nixpkgs = {
      url = "github:NixOS/nixpkgs/5e39a5c1129d6a772175418025f51b0c3022f971";  
    };

    darwin = {
      url = "github:lnl7/nix-darwin/bcc8afd06e237df060c85bad6af7128e05fd61a3";  # master @ 2024-03-17
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/017b12de5b899ef9b64e2c035ce257bfe95b8ae2";  # master @ 2024-03-11
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

  };

  outputs =
    { nixpkgs
    , darwin
    , home-manager
    , emacs-lsp-booster
    , fenix
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
        (import ./Nix/Home/programs/emacs/emacs-patch.nix { inherit pkgs; })
        (final: prev: {
          dockutil = prev.dockutil.overrideAttrs (_: {
            src = let version = "3.1.3";
            in prev.fetchurl {
              url =
                "https://github.com/kcrawford/dockutil/releases/download/${version}/dockutil-${version}.pkg";
                sha256 =
                  "f60db8273fe80d7c4824588bedb538a5387675c3917edb0010dab9f52d3f2582";
            };
          });
        })

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
    homeConfigurations.paul = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [ ./Nix/Home/home.nix ];
    };
  };
}
