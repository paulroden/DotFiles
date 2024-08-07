pkgs:
with pkgs;
let
  elmpkg = elmPackages;
  hasxe = haskell.lib.justStaticExecutables;
  haskg = haskellPackages;
 in
[
  (agda.withPackages (p: [
    p.standard-library
    p.cubical
  ]))
  automake
  awscli2
  bandwhich
  bottom
  broot
  cabal-install
  cabal2nix
  cachix
  cargo-watch
  cctools
  chez
  clang_18
  cmake
  colima
  complgen
  darwin.CF  # CoreFoundation
  darwin.Security
  deno
  dhall
  dhall-bash
  dhall-json
  dhall-lsp-server
  dhall-nix
  dhall-nixpkgs
  direnv
  dockutil
  du-dust
  elmpkg.elm
  elmpkg.elm-format
  elmpkg.elm-language-server
  elmpkg.elm-test
  elmpkg.elm-review
  elmpkg.elm-test-rs
  emacs'
  emacs-lsp-booster
  emacs-vterm
  emacsPackages.pdf-tools
  eud
  exiftool
  eza
  fastfetch
  fd
  ffmpeg
  figlet
  fish
  fzf
  ghciwatch
  gh
  git
  git-ignore
  gnuplot
  go
  graphviz
  haskell.compiler.ghc98
  (haskell-language-server.override {
    supportedGhcVersions = [ "92" "94" "96" "98" ];
  })
  haskg.hoogle
  haskg.shake
  (hasxe haskg.fourmolu)
  (hasxe haskg.happy)
  (hasxe haskg.hlint)
  (hasxe haskg.hpack)
  (hasxe haskg.hscolour)
  (hasxe haskg.pointfree)
  helix
  htop
  httpie
  hunspell
  hyperfine
  hyx
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
  lua53Packages.digestif
  mas
  magic-wormhole-rs
  ncurses
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
    p.pip
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
  pqrs
  procs
  qemu
  racket
  rage
  ripgrep
  ruff
  sd
  shfmt
  silver-searcher
  sioyek
  skhd
  socat
  sniffnet
  srgn
  starship
  tailscale
  tealdeer
  termpdfpy
  texlive.combined.scheme-small
  tmux
  tokei
  tree-sitter
  typst
  typst-lsp
  typstfmt
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
