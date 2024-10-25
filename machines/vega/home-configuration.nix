{ pkgs, lib, ... }:
let
  inherit (lib.hm.gvariant) mkTuple mkUint32;
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
    chromium
    firefox
    vscode
    neovim
    wezterm
    zathura
  ];

  home.stateVersion = "23.05";

  home.file.".ghci".source = ../../.config/.ghci;
  home.file.".npmrc".source = ../../.config/.npmrc;
  home.file.".sqliterc".source = ../../.config/.sqliterc;

  home.sessionPath = [
    "$HOME/.npm-global/bin"
    "$HOME/.local/bin"
  ];

  gtk = {
    enable = true;
  };

  dconf.settings = {
    "org/gnome/desktop/input-sources" = {
      sources = [
        (mkTuple [ "xkb" "us" ])
        (mkTuple [ "xkb" "ru" ])
        (mkTuple [ "xkb" "de" ])
      ];
    };
    "org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      font-name = "SF Pro Display 11";
      document-font-name = "SF Pro Display 11";
      monospace-font-name = "SF Mono Medium 12";
      show-battery-percentage = true;
    };
    "org/gnome/desktop/session" = {
      idle-delay = mkUint32 900;
    };
    "org/gnome/desktop/sound" = {
      event-sounds = false;
    };
    "org/gnome/desktop/wm/keybindings" = {
      close = [ "<Super>q" ];
      move-to-monitor-left = [ "<Shift><Super>Left" ];
      move-to-monitor-right = [ "<Shift><Super>Right" ];
      move-to-workspace-left = [ "<Shift><Super>u" ];
      move-to-workspace-right = [ "<Shift><Super>i" ];
      switch-to-workspace-left = [ "<Super>u" ];
      switch-to-workspace-right = [ "<Super>i" ];
    };
    "org/gnome/desktop/wm/preferences" = {
      button-layout = "appmenu:minimize,maximize,close";
      titlebar-font = "SF Pro Display Bold 11";
    };
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "appindicatorsupport@rgcjonas.gmail.com"
      ];
      favorite-apps = [ ];
    };
    "org/gnome/shell/keybindings" = {
      show-screenshot-ui = [ ];
    };
    "org/gnome/shell/app-switcher" = {
      current-workspace-only = true;
    };
    "org/gnome/shell/extensions/dash-to-dock" = {
      apply-custom-theme = true;
      extend-height = false;
      dash-max-icon-size = 38;
      dock-position = "BOTTOM";
      preview-size-scale = 0.0;
      show-mounts = false;
      show-show-apps-button = false;
      show-trash = false;
    };
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = true;
      night-light-temperature = mkUint32 3700;
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/"
      ];
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
      name = "Browser";
      command = "firefox";
      binding = "<Super>f";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
      name = "Browser Private";
      command = "firefox --private-window";
      binding = "<Shift><Super>f";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3" = {
      name = "Screenshot";
      command = "flameshot gui";
      binding = "Print";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4" = {
      name = "Terminal";
      command = "wezterm";
      binding = "<Super>Return";
    };
    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-type = "nothing";
      sleep-inactive-battery-timeout = 1800;
      sleep-inactive-battery-type = "suspend";
    };
    "org/gnome/mutter" = {
      center-new-windows = true;
      dynamic-workspaces = true;
      workspaces-only-on-primary = true;
    };
    "org/gnome/nautilus/icon-view" = {
      captions = [ "size" "none" "none" ];
      default-zoom-level = "small-plus";
    };
    "org/gnome/nautilus/window-state" = {
      maximized = true;
    };
    "org/gnome/nautilus/preferences" = {
      default-folder-viewer = "icon-view";
    };
    "org/gtk/gtk4/settings/file-chooser" = {
      sort-directories-first = true;
    };
  };

  home.packages = with pkgs; [
    ticktick
    telegram-desktop
    zulip
    zulip-term
    discord
    keepassxc
    stacer
    qbittorrent
    transmission-gtk
    openvpn
    baobab
    smartmontools
    gcolor3
    flameshot
    libqalculate
    nfs-utils
    gnomeExtensions.appindicator
    zoom-us
    # CLI
    difftastic
    dig
    fzf
    htop
    jq
    pfetch
    rename
    ripgrep
    rlwrap
    solaar
    spek
    tmux
    tokei
    tree
    # Dev
    insomnia
    # Documents
    anki
    notes
    obsidian
    hledger
    hledger-web
    mdbook
    mdbook-toc
    poppler_utils
    newsboat
    nb
    texlive.combined.scheme-medium
    hugo
    zed-editor
    wpsoffice
    # Media
    moc
    ffmpeg-full
    mpv
    yt-dlp
    feh
    imagemagick
    obs-studio
    optipng
    peek
    krita
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
    # C
    gcc
    gdb
    llvmPackages_16.clang-unwrapped
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
    # Java
    maven
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
    # TLA+
    tlaps
    tlaplus
  ];
}
