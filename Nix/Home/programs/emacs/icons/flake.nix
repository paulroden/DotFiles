{
  description = "Nobuyuki Sato's Squricley-beautiful Emacs Icon, Derived for Building Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; };
    in
      {
        packages = {
          default = pkgs.stdenv.mkDerivation {
            name = "nobu417-emacs-icon";

            src = pkgs.fetchgit {
              url = "https://github.com/nobu417/emacs-icon-replacement-for-macos-big-sur.git";
              rev = "67630420d3fc979d4deef8e38b8426d6ad9c6256";
              sha256 = "abc";
            };

            buildPhase = ''
            mkdir -p $out/icons
            cp ${src}/Emacs.icns $out/icons/

            source_png=$out/emacs.png
            sizes=(16 24 32 48 128)
            for size in "${sizes[@]}"; do
              output_to="$out/icons/${size}x${size}/apps"
              mkdir -p "${output_to}"
              convert "$out/emacs.png" -resize "${size}x${size}" "${output_to}"/apps/emacs.png   
            done
          '';          
          };
        };
      }


}
