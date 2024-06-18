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
  bandwhich
  bottom
  broot
  cabal-install
  cabal2nix
  cachix
  cargo-watch
  chez-racket
  clang_18
  cmake
  colima
  complgen
  darwin.apple_sdk.frameworks.CoreFoundation
  darwin.apple_sdk.frameworks.Security
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
  emacs'
  emacs-lsp-booster
  emacs-vterm
  emacsPackages.pdf-tools
  eud
  exiftool
  eza
  fd
  ffmpeg
  figlet
  fish
  fzf
  gh
  git
  git-ignore
  gnuplot
  go
  graphviz
  haskell.compiler.ghc910
  haskg.hoogle
  haskg.shake
  (hasxe haskg.fourmolu)
  (hasxe haskg.happy)
  (hasxe haskg.hlint)
  (hasxe haskg.hpack)
  (hasxe haskg.hscolour)
  (hasxe haskg.pointfree)
  helix
  # hexcurse
  htop
  httpie
  hunspell
  hyperfine
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
#  srgn
  starship
  termpdfpy
  texlive.combined.scheme-small
  tlrc
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
  # for here:  , , , , , 
  # for cargo:  gign, eud
  
]
