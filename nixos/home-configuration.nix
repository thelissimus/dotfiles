{ pkgs, lib, ... }:
let
  inherit (lib.hm.gvariant) mkTuple mkUint32;
in
{
  home.stateVersion = "23.05";

  home.file.".local/share/fonts/SFMono".source = ../.fonts/SFMono;
  home.file.".local/share/fonts/SFProDisplay".source = ../.fonts/SFProDisplay;

  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.firefox = {
    enable = true;
    profiles.default = {
      isDefault = true;
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        clearurls
        darkreader
        # dont-accept-image-webp
        # imagus
        i-dont-care-about-cookies
        simple-tab-groups
        single-file
        ublock-origin
        unpaywall
        vimium
        youtube-shorts-block
        # user-agent-switcher-manager
      ];
      settings = {
        "browser.newtabpage.activity-stream.feeds.topsites" = false;
        "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = false;
        "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = false;
        "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
        "browser.newtabpage.activity-stream.section.highlights.includeVisited" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.search.region" = "DE";
        "browser.search.suggest.enabled" = false;
        "browser.toolbars.bookmarks.visibility" = "never";
        "browser.urlbar.showSearchSuggestionsFirst" = false;
        "browser.urlbar.suggest.searches" = false;
        "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
        "signon.autofillForms" = false;
        "signon.firefoxRelay.feature" = "disabled";
        "signon.generation.enabled" = false;
        "signon.management.page.breach-alerts.enabled" = false;
        "signon.rememberSignons" = false;
      };
    };
  };
  programs.vscode = {
    enable = true;
    enableExtensionUpdateCheck = false;
    enableUpdateCheck = false;
    package = pkgs.vscode-fhs;
    extensions = with pkgs.vscode-extensions; [
      llvm-vs-code-extensions.vscode-clangd
      editorconfig.editorconfig
      usernamehw.errorlens
      dbaeumer.vscode-eslint
      tamasfe.even-better-toml
      # flix.flix
      donjayamanne.githistory
      golang.go
      jdinhlife.gruvbox
      haskell.haskell
      justusadam.language-haskell
      sumneko.lua
      # carlos-algms.make-task-provider
      # bierner.markdown-preview-github-styles
      pkief.material-icon-theme
      pkief.material-product-icons
      jnoortheen.nix-ide
      christian-kohler.path-intellisense
      esbenp.prettier-vscode
      chenglou92.rescript-vscode
      scalameta.metals
      scala-lang.scala
      timonwong.shellcheck
      vscodevim.vim
      zxh404.vscode-proto3
    ];
    keybindings = builtins.fromJSON (builtins.readFile ../.config/Code/User/keybindings.json);
    userSettings = builtins.fromJSON (builtins.readFile ../.config/Code/User/settings.json);
  };
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    plugins = with pkgs.vimPlugins; [
      # LSP
      nvim-lspconfig
      nvim-cmp
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      cmp-vsnip
      vim-vsnip
      # Modification
      editorconfig-vim
      nvim-treesitter
      nvim-treesitter-parsers.haskell
      nvim-treesitter-parsers.nix
      nvim-treesitter-parsers.rust
      # Components
      diffview-nvim
      nvim-tree-lua
      fzf-vim
      tagbar
      undotree
      # Appearance
      gruvbox-material
      nvim-web-devicons
      lightline-vim
      nvim-lightline-lsp
    ];
    extraConfig = ''
      ${builtins.readFile ../.config/nvim/init.vim}
      lua << EOF
      ${builtins.readFile ../.config/nvim/lua/lsp.lua}
      EOF
    '';
  };
  programs.wezterm = {
    enable = true;
    extraConfig = builtins.readFile ../.config/wezterm/wezterm.lua;
  };

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
    anki
    telegram-desktop
    keepassxc
    qbittorrent
    baobab
    smartmontools
    gcolor3
    flameshot
    libqalculate
    qalculate-gtk
    rename
    rlwrap
    ripgrep
    fzf
    tmux
    tokei
    tree
    nicotine-plus
    htop
    nvtop
    onefetch
    pfetch
    insomnia
    pinentry
    difftastic
    git
    gnome.gnome-tweaks
    gnomeExtensions.appindicator
    # Editor
    jetbrains.idea-community
    jetbrains.datagrip
    # Documents
    doxygen
    obsidian
    zathura
    poppler_utils
    newsboat
    # Media
    ffmpeg
    mpv
    vlc
    yt-dlp
    obs-studio
    cmus
    feh
    imagemagick
    optipng
    peek
    # Database
    sqlite
    # Tools
    android-tools
    # C
    gcc
    gdb
    tinycc
    llvmPackages_16.clang-unwrapped
    # Clojure
    clojure
    leiningen
    # Go
    go
    gopls
    delve
    go-tools
    # Haskell
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.cabal-fmt
    haskellPackages.fourmolu
    haskellPackages.hlint
    # Java
    maven
    jdk17
    # Nix
    nil
    # JS
    nodejs_20
    nodePackages.pnpm
    # Scala
    metals
    dotty
    sbt
    scalafmt
    scalafix
    scala-cli
  ];
}
