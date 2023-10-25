{ pkgs, lib, ... }:
let
  inherit (lib.hm.gvariant) mkTuple mkUint32;
in
{
  home.stateVersion = "23.05";

  home.file.".local/share/fonts/SFMono".source = ../../.fonts/SFMono;
  home.file.".local/share/fonts/SFProDisplay".source = ../../.fonts/SFProDisplay;
  home.file.".ghci".source = ../../.config/.ghci;
  home.file.".npmrc".source = ../../.config/.npmrc;
  home.file.".sqliterc".source = ../../.config/.sqliterc;

  programs.zoxide.enable = true;
  programs.bash.enable = true;
  programs.command-not-found.enable = true;
  programs.git.difftastic.enable = true;
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;

    history = {
      extended = true;
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
      theme = "robbyrussell";
    };

    shellAliases = {
      ydl = "yt-dlp -o '%(title)s.%(ext)s' -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best'";
    };
  };
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
        privacy-badger
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
        "font.name.monospace.x-western" = "SF Mono";
        "font.name.sans-serif.x-western" = "SF Pro Display";
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
    package = pkgs.vscode;
    extensions = with pkgs.vscode-extensions; [
      betterthantomorrow.calva
      llvm-vs-code-extensions.vscode-clangd
      editorconfig.editorconfig
      usernamehw.errorlens
      dbaeumer.vscode-eslint
      tamasfe.even-better-toml
      donjayamanne.githistory
      golang.go
      jdinhlife.gruvbox
      haskell.haskell
      justusadam.language-haskell
      james-yu.latex-workshop
      sumneko.lua
      pkief.material-icon-theme
      pkief.material-product-icons
      jnoortheen.nix-ide
      ocamllabs.ocaml-platform
      christian-kohler.path-intellisense
      esbenp.prettier-vscode
      chenglou92.rescript-vscode
      scalameta.metals
      scala-lang.scala
      timonwong.shellcheck
      vscodevim.vim
      zxh404.vscode-proto3
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "erlang-ls";
        publisher = "erlang-ls";
        version = "0.0.40";
        sha256 = "HFlOig5UUsT+XX0h1dcRQ3mWRsASqvKTMpqqRhVpTAY=";
      }
      {
        name = "lean4";
        publisher = "leanprover";
        version = "0.0.111";
        sha256 = "c6d+rNE1j0NLx+ntGU6E9G1d9S0pxKYBzfnb8Je/mkE=";
      }
      {
        name = "markdown-checkbox";
        publisher = "bierner";
        version = "0.4.0";
        sha256 = "AoPcdN/67WOzarnF+GIx/nans38Jan8Z5D0StBWIbkk=";
      }
      {
        name = "markdown-preview-github-styles";
        publisher = "bierner";
        version = "2.0.3";
        sha256 = "yuF6TJSv0V2OvkBwqwAQKRcHCAXNL+NW8Q3s+dMFnLY=";
      }
      {
        name = "typescript-explorer";
        publisher = "mxsdev";
        version = "0.4.2";
        sha256 = "IHz7fpE+RiLP6tEkWwShsfDPM3rTbq5tE9/BHt1QkIQ=";
      }
    ];
    keybindings = builtins.fromJSON (builtins.readFile ../../.config/Code/User/keybindings.json);
    userSettings = builtins.fromJSON (builtins.readFile ../../.config/Code/User/settings.json);
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
      (nvim-treesitter.withPlugins
        (p: with p; [ c erlang go haskell haskell_persistent latex nix ocaml ocaml_interface scala ]))
      # Components
      diffview-nvim
      nvim-tree-lua
      fzf-vim
      tagbar
      undotree
      vimtex
      # Appearance
      gruvbox-material
      nvim-web-devicons
      lightline-vim
      nvim-lightline-lsp
    ];
    extraConfig = ''
      ${builtins.readFile ../../.config/nvim/init.vim}
      lua << EOF
      ${builtins.readFile ../../.config/nvim/lua/lsp.lua}
      EOF
    '';
  };
  programs.wezterm = {
    enable = true;
    extraConfig = builtins.readFile ../../.config/wezterm/wezterm.lua;
  };
  programs.zathura = {
    enable = true;
    options = {
      font = "SF Mono 10";
      selection-clipboard = "clipboard";
      window-title-basename = true;
    };
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

  home.packages =
    let
      hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ "92" ]; };
    in
    with pkgs; [
      anki
      telegram-desktop
      keepassxc
      stacer
      qbittorrent
      openvpn
      baobab
      smartmontools
      gcolor3
      flameshot
      libqalculate
      qalculate-gtk
      nicotine-plus
      pinentry
      gnome.gnome-tweaks
      gnomeExtensions.appindicator
      # CLI
      difftastic
      fzf
      htop
      jq
      pfetch
      rename
      ripgrep
      rlwrap
      tmux
      tokei
      tree
      yazi
      # Dev
      git
      insomnia
      # Documents
      hledger
      hledger-ui
      doxygen
      obsidian
      poppler_utils
      pandoc
      newsboat
      nb
      texlive.combined.scheme-medium
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
      libz
      # C
      gcc
      gdb
      tinycc
      llvmPackages_16.clang-unwrapped
      # Clojure
      clojure
      leiningen
      # Erlang
      erlang
      erlang-ls
      rebar3
      # Go
      go
      gopls
      delve
      go-tools
      # Haskell
      haskell.compiler.ghc92
      cabal-install
      hls
      haskellPackages.hls-cabal-plugin
      haskellPackages.hls-cabal-fmt-plugin
      haskellPackages.hls-eval-plugin
      haskellPackages.cabal-fmt
      haskellPackages.fourmolu
      haskellPackages.hlint
      haskellPackages.hoogle
      # Java
      maven
      jdk17
      # JS
      bun
      nodejs_20
      nodePackages.pnpm
      # Nix
      nil
      nixpkgs-fmt
      # Lean
      lean4
      # OCaml
      ocaml
      opam
      dune_3
      ocamlPackages.merlin
      ocamlPackages.ocaml-lsp
      ocamlPackages.odoc
      ocamlPackages.ocamlformat
      # Scala
      metals
      dotty
      sbt
      scala-cli
    ];
}
