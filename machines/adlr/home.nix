{ pkgs, ... }:
{
  imports = [
    ../../modules/home/zoxide.nix
    ../../modules/home/bash.nix
    ../../modules/home/git.nix
    ../../modules/home/java.nix
    ../../modules/home/scala.nix
    ../../modules/home/zsh.nix
    ../../modules/home/firefox.nix
    ../../modules/home/vscode.nix
    ../../modules/home/neovim.nix
    ../../modules/home/wezterm.nix
    ../../modules/home/zathura.nix
  ];

  home.stateVersion = "24.05";

  home.packages = with pkgs; [
    ticktick
    telegram-desktop
    zulip
    discord
    keepassxc
    libqalculate
    zoom-us
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
    # Dev
    insomnia
    # Documents
    anki
    obsidian
    mdbook
    mdbook-toc
    texlive.combined.scheme-medium
    hugo
    zed-editor
    # Media
    ffmpeg-full
    yt-dlp
    feh
    obs-studio
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
