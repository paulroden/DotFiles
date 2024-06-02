{
  description =
    "Nobuyuki Sato's Squricley-beautiful Emacs Icon, Derived for Building Emacs";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/5e39a5c1129d6a772175418025f51b0c3022f971";
    flake-utils.url =
      "github:numtide/flake-utils/b1d9ab70662946ef0850d488da1c9019f3a9752a";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = {
          default = pkgs.stdenv.mkDerivation {
            name = "nobu417-emacs-icon";
            src = pkgs.fetchgit {
              url =
                "https://github.com/nobu417/emacs-icon-replacement-for-macos-big-sur.git";
              rev = "67630420d3fc979d4deef8e38b8426d6ad9c6256";
              sha256 = "0lwdk697229931blhyzz498c75gm2h27w9b0m8fk4973fvn2k4mw";
            };

            nativeBuildInputs = [ pkgs.imagemagick ];

            buildPhase = ''
              mkdir -p $out/icons
              cp $src/Emacs.icns $out/icons/
              cp $src/emacs.png $out/icons/

              mkdir -p "$out/icons/16x16/apps"; convert $src/emacs.png -resize 16x16 "$out/icons/16x16/apps/emacs.png"
              mkdir -p "$out/icons/24x24/apps"; convert $src/emacs.png -resize 24x24 "$out/icons/24x24/apps/emacs.png"
              mkdir -p "$out/icons/32x32/apps"; convert $src/emacs.png -resize 32x32 "$out/icons/32x32/apps/emacs.png"
              mkdir -p "$out/icons/48x48/apps"; convert $src/emacs.png -resize 48x48 "$out/icons/48x48/apps/emacs.png"
              mkdir -p "$out/icons/128x128/apps"; convert $src/emacs.png -resize 128x128 "$out/icons/128x128/apps/emacs.png"
            '';
          };
        };
      });
}
