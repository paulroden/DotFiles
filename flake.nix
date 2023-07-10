{
  description = "A Nix Home Manager Configuration for Paul Roden Henderson";

  inputs = {

    flake-utils = { url = "github:numtide/flake-utils"; };

    nixpkgs = {
      url = "github:NixOS/nixpkgs/bf0e44973fd10199eb47325b94647d84b0317168";  # nixpkgs-unstable
    };

    nixpkgs-head = {
      url = "github:NixOS/nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    home-manager = {
      url = "github:nix-community/home-manager/07c347bb50994691d7b0095f45ebd8838cf6bc38"; # release 23.05
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin-emacs = {
      url = "github:paulroden/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin-emacs-packages = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    soft-serve = {
      url = "github:paulroden/soft-serve";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    # crane for Rust, because it lifts cargo crates 🦀
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
  };

  outputs =
    inputs@{
      nixpkgs
    , darwin
    , home-manager
    , darwin-emacs
    , darwin-emacs-packages
    , haskell-nix
    , fenix
    , soft-serve
    , ...
    }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        localSystem = system;
        overlays = [
          darwin-emacs.overlays.emacs
          darwin-emacs-packages.overlays.package
          fenix.overlays.default
          soft-serve.overlays.default
          (
            final: prev: {
              head = import inputs.nixpkgs-head { system = final.system; };
            }
          )
        ];
        config.allowUnfree = true;
      };
    in {
      packages.${system}.default = fenix.packages.${system}.minimal.toolchain;
      darwinConfigurations = {
        "Asara" = darwin.lib.darwinSystem {
          inherit pkgs system;
          inputs = { inherit darwin home-manager nixpkgs pkgs; };
          modules = [
            ./Nix/Devices/Asara
          ];
        };
      };
      homeConfigurations.paul = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./Nix/Home/home.nix
        ];
      };
    };
}
