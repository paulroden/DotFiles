pkgs:
[
  (pkgs.agda.withPackages (p: [
    p.standard-library
    p.cubical
  ]))
  pkgs.automake
  pkgs.broot
  pkgs.cabal-install
  pkgs.cabal2nix
  pkgs.cachix
  pkgs.chez-racket
  pkgs.clang_14
  pkgs.cmake
  pkgs.deno
  pkgs.dhall
  pkgs.dhall-bash
  pkgs.dhall-json
  pkgs.dhall-lsp-server
  pkgs.dhall-nix
  pkgs.dhall-nixpkgs
  pkgs.discord  # UNFREE
  pkgs.direnv
  pkgs.dockutil
  pkgs.du-dust
  pkgs.emacs-vterm
  pkgs.emacsGit
  pkgs.exa
  pkgs.exiftool
  pkgs.fd
  pkgs.ffmpeg
  pkgs.figlet
  pkgs.fish
  pkgs.fzf
  pkgs.gh
#  pkgs.ghostscript  # conflict with texlive
  pkgs.git
  pkgs.go
  pkgs.graphviz
  pkgs.haskell.compiler.ghc94
  # pkgs.haskellPackages.haskell-language-server # .override {
  #   supportedGhcVersions = [ "810" "902" "944" ];
  # }
  pkgs.helix
  # pkgs.hexcurse
  pkgs.hpack
  pkgs.htop
  pkgs.httpie
  pkgs.hunspell
  pkgs.iina
  pkgs.imgcat
  pkgs.ispell
  pkgs.jq
  pkgs.jql
  pkgs.jupyter
  pkgs.less
  pkgs.lima
  pkgs.libgit2
  pkgs.libiconv
  pkgs.llvmPackages.bintools-unwrapped # ref. https://matklad.github.io/2022/03/14/rpath-or-why-lld-doesnt-work-on-nixos.html; `-unwrapped` avoids collision with pkgs.clang
  pkgs.lua53Packages.digestif
  pkgs.mas
  pkgs.magic-wormhole-rs
  pkgs.ncurses
  pkgs.neofetch
  pkgs.nil
  pkgs.nix-prefetch-git
  pkgs.nix-tree
  pkgs.nixfmt
  pkgs.nix-your-shell
  pkgs.nodejs
  pkgs.nodePackages.bash-language-server
  # pkgs.ormolu
  pkgs.ouch
  pkgs.pandoc
  pkgs.poetry
  (pkgs.python311.withPackages(p: [
    p.httpx
    p.ipython
    p.mypy
    p.mypy-extensions
    p.numpy
    p.pandas
    #  p.polars  --currently broken on Darwin and aarch64 linux
    p.poetry-core
    p.pydantic
  ]))
  pkgs.procs
  pkgs.qemu
  pkgs.racket
  pkgs.ripgrep
  pkgs.ruff
  pkgs.rust-analyzer
  pkgs.sd
  pkgs.shfmt
  pkgs.silver-searcher
  pkgs.slack  # UNFREE
  pkgs.socat
  pkgs.starship
  pkgs.teams  # UNFR€€
  pkgs.texlive.combined.scheme-full
  pkgs.tldr
  pkgs.tmux
  pkgs.tree-sitter
  pkgs.watchexec
  pkgs.wget
  pkgs.xsv
  pkgs.yq
  pkgs.zlib
  pkgs.zoom-us  # UNFREE
  pkgs.zulip-term
  pkgs.zsh
  pkgs.zsh-autocomplete
  pkgs.zsh-completions
  pkgs.zstd
]
