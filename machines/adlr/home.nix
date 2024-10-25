{ pkgs, ... }:
let
  hp = import ../../modules/home { inherit pkgs; };
in
{
  imports = with hp; [
    zoxide
    bash
    git
    java
    scala
    zsh
    neovim
  ];

  home.stateVersion = "24.05";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    gnupg
    pinentry_mac
    libqalculate
    # CLI
    difftastic
    dig
    fzf
    htop
    jq
    pfetch
    ripgrep
    rlwrap
    spek
    tmux
    tokei
    tree
    # Documents
    mdbook
    mdbook-toc
    texlive.combined.scheme-medium
    hugo
    # Media
    ffmpeg-full
    yt-dlp
    feh
    optipng
    # Database
    sqlite
    # Agda
    (agda.withPackages (ps: with ps; [
      standard-library
    ]))
    # Alloy
    alloy6
    # BQN
    cbqn
    # Go
    go
    gopls
    delve
    go-tools
    # Haskell
    haskell.compiler.ghc910
    cabal-install
    (haskell-language-server.override { supportedGhcVersions = [ "910" ]; supportedFormatters = [ "fourmolu" ]; })
    haskellPackages.cabal-fmt
    haskellPackages.fourmolu
    haskellPackages.hlint
    haskellPackages.ghcprofview
    # JS
    nodejs_20
    nodePackages.pnpm
    # Nix
    nil
    nixd
    nixpkgs-fmt
    # Lean
    elan
    # Prolog
    swiProlog
    # Rust
    rustup
    # Scala
    metals
  ];
}
