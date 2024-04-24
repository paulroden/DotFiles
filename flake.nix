{
  description = "A Nix Home Manager Configuration for Paul Roden Henderson";

  inputs = {

    flake-utils = { url = "github:numtide/flake-utils"; };

    nixpkgs = {
      url = "github:NixOS/nixpkgs/5e39a5c1129d6a772175418025f51b0c3022f971";  
    };

    darwin = {
      url = "github:lnl7/nix-darwin/bcc8afd06e237df060c85bad6af7128e05fd61a3";  # master @ 2024-03-17
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url =
        "github:nix-community/home-manager/017b12de5b899ef9b64e2c035ce257bfe95b8ae2";  # master @ 2024-03-11
        inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    soft-serve = {
      url = "github:paulroden/soft-serve";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fenix = {
      url = "github:nix-community/fenix/12619df460ea671b1e94a5c2c8c17ca91cb86ebe";  # master @ 2024-03-25
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # crane for Rust, because it lifts cargo crates 🦀
    crane = {
      url = "github:ipetkov/crane/v0.16.3";
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
    , ... }:
  let
    system = "aarch64-darwin";
    pkgs = import nixpkgs {
      localSystem = system;
      overlays = [
        emacs-lsp-booster.overlays.default
        fenix.overlays.default
        soft-serve.overlays.default
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
