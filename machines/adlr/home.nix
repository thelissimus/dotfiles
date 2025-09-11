{ pkgs, lib, ... }:
let
  hp = import ../../modules/home { inherit pkgs lib; };
in
{
  imports = with hp; [
    zoxide
    bash
    direnv
    git
    haskell
    java
    zsh
    neovim
    zathura
    vscode
  ];

  home.stateVersion = "24.05";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    gnupg
    pinentry_mac
    libqalculate
    # CLI
    difftastic
    htop
    jq
    ripgrep
    rlwrap
    spek
    tokei
    tree
    just
    # Documents
    mdbook
    mdbook-toc
    texlive.combined.scheme-medium
    # Media
    ffmpeg-full
    yt-dlp
    optipng
    # Database
    sqlite
    # Agda
    (agda.withPackages (ps: with ps; [
      standard-library
      cubical
      _1lab
      generics
      cubical-mini
    ]))
    cornelis
    # Alloy
    alloy6
    # K Framework
    # k
    # Coq
    coq
    coqPackages.stdpp
    coqPackages.iris
    coqPackages.HoTT
    coqPackages.mathcomp
    coqPackages.coq-lsp
    # OCaml
    ocaml
    opam
    dune_3
    ocamlPackages.merlin
    ocamlPackages.ocaml-lsp
    ocamlPackages.odoc
    ocamlPackages.ocamlformat
    # JS
    nodejs_20
    nodePackages.pnpm
    # Nix
    nil
    nixd
    nixpkgs-fmt
    # Lean
    elan
    z3
    llvmPackages_15.clang-unwrapped
    llvmPackages_15.libllvm
    # Zig
    zig
    zls
  ];
}
