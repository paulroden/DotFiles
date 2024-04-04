pkgs:
with pkgs;
let haskellExe = x: haskell.lib.justStaticExecutables haskellPackages.${x};
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
  emacs-lsp-booster
  emacs-vterm
  emacsGit
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
  (haskellExe "happy")
  (haskellExe "hlint")
  (haskellExe "hoogle")
  (haskellExe "hpack")
  (haskellExe "hscolour")
  (haskellExe "pointfree")
  (haskellExe "shake")
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
  less
  lima
  libgit2
  libiconv
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
  # ormolu
  obsidian
  onefetch
  ouch
  pandoc
  poetry
  (head.python311.withPackages(p: [
    p.httpx
    p.ipython
 #   p.jupyterlab
    p.mypy
    p.mypy-extensions
    p.numpy
    p.pandas
    # p.polars
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
  slack  # UNFREE
  skhd
  socat
  starship
  teams  # UNFR€€
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
  zoom-us  # UNFREE
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
