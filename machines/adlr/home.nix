{ pkgs, lib, ... }:
let
  hp = import ../../modules/home { inherit pkgs lib; };
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
    zathura
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
      cubical
      _1lab
      generics
      cubical-mini
      pkgs.agda2hs-lib
    ]))
    agda2hs-unwrapped
    cornelis
    # Idris 2
    idris2
    idris2Packages.idris2Lsp
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
    stack
    (haskell-language-server.override { supportedGhcVersions = [ "910" ]; supportedFormatters = [ "fourmolu" ]; })
    haskellPackages.cabal-fmt
    haskellPackages.fourmolu
    haskellPackages.hlint
    haskellPackages.ghcprofview
    # K Framework
    k
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
    # Prolog
    swi-prolog
    # Rust
    rustup
    # Scala
    metals
    z3
    llvmPackages_15.clang-unwrapped
    llvmPackages_15.libllvm
  ];
}

/*
ca-certificates
gettext
gmp
gnu-time
icu4c@76
krb5
libunistring
lz4
ntfs-3g-mac
ollama
openssl@3
postgresql@15
readline
redis
xz
zlib
zstd
alloy
alt-tab
anki
betterdisplay
chatgpt
claude
datagrip
deepl
discord
docker
elmedia-player
eloston-chromium
figma
firefox
floorp
font-iosevka
font-martian-mono
font-mononoki
font-sf-mono
ghostty
gitbutler
iterm2
jetbrains-toolbox
keepassxc
keka
librewolf
lm-studio
logi-options+
macfuse
mumble
obs
obsidian
orbstack
osu
prismlauncher
qbittorrent
sioyek
sketch
steam
syncplay
telegram
termius
ticktick
unnaturalscrollwheels
visual-studio-code
vlc
wezterm
zed
zen-browser
zoom
zulip
*/
