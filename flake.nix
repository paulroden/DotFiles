{
  description = "A Nix Home Manager Configuration for Paul Roden Henderson";

  inputs = {

    flake-utils = { url = "github:numtide/flake-utils"; };

    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    home-manager = {
      url = "github:nix-community/home-manager";
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
  
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    # crane for Rust, because it lifts cargo crates 🦀
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    nix-your-shell = {
      url = "github:MercuryTechnologies/nix-your-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs
    , darwin
    , home-manager
    , darwin-emacs
    , darwin-emacs-packages
    , haskell-nix
    , nix-your-shell
    , ...
    }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        localSystem = system;
        overlays = [
          darwin-emacs.overlays.emacs
          darwin-emacs-packages.overlays.package
          nix-your-shell.overlays.default
        ];
        config.allowUnfree = true;
      };
    in {
      darwinConfigurations = {
        "Asara" = darwin.lib.darwinSystem {
          inherit system;
          inputs = { inherit darwin home-manager nixpkgs; };
          modules = [
            ./Nix/Devices/Asara.nix
            ./Nix/Impure/homebrew.nix
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
