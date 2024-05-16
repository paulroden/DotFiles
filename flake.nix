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
        (final: prev: {
          emacs' = prev.emacs.overrideAttrs (prev: {
            # Use the beautiful squircle icon from Noboyuki Sato
            postUnpack = (prev.postUnpack or "") + ''
              cp ${./Nix/Home/programs/emacs/nobu417-big-sur.icns} $sourceRoot/nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
              '';
            patches = (prev.patches or []) ++ [
              # Fix OS window role (needed for window managers like yabai)
              (pkgs.fetchpatch {
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
                sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
              })
              # Use poll instead of select to get file descriptors
              (pkgs.fetchpatch {
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
                sha256 = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
              })
              # leave frame selecting to Emacs
              (pkgs.fetchpatch {
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-frame-refocus-cocoa.patch";
                sha256 = "sha256-QLGplGoRpM4qgrIAJIbVJJsa4xj34axwT3LiWt++j/c=";
              })
              # Make Emacs aware of OS-level light/dark mode
              (pkgs.fetchpatch {
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
                sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
              })
            ];
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
