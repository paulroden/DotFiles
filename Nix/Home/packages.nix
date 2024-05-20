pkgs:
with pkgs;
let
  hasxe = haskell.lib.justStaticExecutables;
  haskg = haskellPackages;
 in
[
  (agda.withPackages (p: [
    p.standard-library
    p.cubical
  ]))
  automake
  broot
  cabal-install
  cabal2nix
  cachix
  chez-racket
  clang_17
  cmake
  colima
  darwin.apple_sdk.frameworks.CoreFoundation
  darwin.apple_sdk.frameworks.Security
  deno
  dhall
  dhall-bash
  dhall-json
  dhall-lsp-server
  dhall-nix
  dhall-nixpkgs
  discord  # UNFREE
  direnv
  dockutil
  du-dust
  emacs'
  emacs-lsp-booster
  emacs-vterm
  emacsPackages.pdf-tools
  exiftool
  eza
  fd
  ffmpeg
  figlet
  fish
  fzf
  gh
  git
  gnuplot
  go
  graphviz
  haskell.compiler.ghc96
  (hasxe haskg.fourmolu)
  (hasxe haskg.happy)
  (hasxe haskg.hlint)
  (hasxe haskg.hoogle)
  (hasxe haskg.hpack)
  (hasxe haskg.hscolour)
  (hasxe haskg.pointfree)
  (hasxe haskg.shake)
  helix
  # hexcurse
  htop
  httpie
  hunspell
  iina
  imgcat
  ispell
  jq
  jql
  jujutsu
  less
  lima
  libgit2
  libiconv
  libpng
  llvmPackages.bintools-unwrapped # ref. https://matklad.github.io/2022/03/14/rpath-or-why-lld-doesnt-work-on-nixos.html; `-unwrapped` avoids collision gwith clang
  lua53Packages.digestif
  mas
  magic-wormhole-rs
  ncurses
  neofetch
  nil
  nix-prefetch-git
  nix-tree
  nixfmt
  nix-your-shell
  nodejs
  nodePackages.bash-language-server
  nushell
  obsidian
  onefetch
  ouch
  pandoc
  poetry
  (python312.withPackages(p: [
    p.httpx
    p.ipython
    p.jupyterlab
    p.numpy
    p.pandas
    p.polars
    p.poetry-core
    p.pyarrow
    p.pydantic
  ]))
  procs
  qemu
  racket
  ripgrep
  ruff
  sd
  shfmt
  silver-searcher
  sioyek
  skhd
  socat
  starship
  termpdfpy
  texlive.combined.scheme-small
  tldr
  tmux
  tree-sitter
  watchexec
  wget
  xsv
  yq
  zlib
  zulip-term
  zsh
  zsh-autocomplete
  zsh-completions
  zstd
  # A rust toolchain via fenix
  (fenix.complete.withComponents [
    "cargo"
    "clippy"
    "rust-src"
    "rustc"
    "rustfmt"
  ])
  rust-analyzer-nightly
]
