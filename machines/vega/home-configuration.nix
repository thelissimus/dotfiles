{ pkgs, lib, ... }:
let
  inherit (lib.hm.gvariant) mkTuple mkUint32;
in
{
  home.stateVersion = "23.05";

  home.file.".ghci".source = ../../.config/.ghci;
  home.file.".npmrc".source = ../../.config/.npmrc;
  home.file.".sqliterc".source = ../../.config/.sqliterc;

  home.sessionPath = [
    "$HOME/.npm-global/bin"
    "$HOME/.local/bin"
  ];

  programs.zoxide.enable = true;
  programs.bash.enable = true;
  programs.git = {
    enable = true;
    lfs.enable = true;
    difftastic.enable = true;
  };
  programs.java = {
    enable = true;
    package = pkgs.jdk17;
  };
  programs.sbt.enable = true;
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    plugins = [{
      name = "vi-mode";
      src = pkgs.zsh-vi-mode;
      file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
    }];

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
      adl = "yt-dlp -o '%(title)s.%(ext)s' -f 'bestaudio[ext=m4a]/best' --extract-audio";
    };
  };
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions =
      let
        mkChromiumExtensionFor = browserVersion: { id, sha256, version }: {
          inherit id;
          crxPath = builtins.fetchurl {
            url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
            name = "${id}.crx";
            inherit sha256;
          };
          inherit version;
        };
        mkChromiumExtension = mkChromiumExtensionFor (lib.versions.major pkgs.ungoogled-chromium.version);
      in
      map mkChromiumExtension [
        {
          # clearurls
          id = "lckanjgmijmafbedllaakclkaicjfmnk";
          version = "1.26.0";
          sha256 = "06m3b3npis7cpv0yif0rs3dkfdgd69r0rkyxlwwry26h58dp7hdc";
        }
        {
          # dark-reader
          id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
          version = "4.9.95";
          sha256 = "0169harv9niwsjhi8pn3p063k1yhnlgjspih6gcfa6wxil50djzp";
        }
        {
          # imagus
          id = "immpkjjlgappgfkkfieppnmlhakdmaab";
          version = "0.9.8.74";
          sha256 = "19mirfy8ggq2zaxp8clv28aq1lmv5xdlvf9j62ig9p75pr4v3qa1";
        }
        {
          # i-still-dont-care-about-cookies
          id = "edibdbjcniadpccecjdfdjjppcpchdlm";
          version = "1.1.4";
          sha256 = "11k7cxcjafs8ziaxl4bilbfwbgl2yf1p6v1bvwszadcr14xyvgsj";
        }
        {
          # privacy-badger
          id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
          version = "2024.7.17";
          sha256 = "0jsqa7v2zdjwwp4gfl98yda6vsii374fl1bwqjynnilj7ah8610z";
        }
        {
          # singlefile
          id = "mpiodijhokgodhhofbcjdecpffjipkle";
          version = "1.22.71";
          sha256 = "12c9j6bfbdnqdb2ncnqwh9rywk351japbrxrfaijx8d1kjcs222m";
        }
        {
          # ublock-origin
          id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
          version = "1.60.0";
          sha256 = "1h7hkch680kp40mrc1121l8rl1qxwplqwbl53ysy5kbp9jn77v1r";
        }
        {
          # unhook
          id = "khncfooichmfjbepaaaebmommgaepoid";
          version = "1.6.8";
          sha256 = "1wwz47zb11ybgi025pbwi911g3ddzv0pkvgybgddxdnjp874xs4f";
        }
        {
          # vimium
          id = "dbepggeogbaibhgnhhndojpepiihcmeb";
          version = "2.1.2";
          sha256 = "0m8xski05w2r8igj675sxrlkzxlrl59j3a7m0r6c8pwcvka0r88d";
        }
        {
          # return-youtube-dislike
          id = "gebbhagfogifgggkldgodflihgfeippi";
          version = "3.0.0.17";
          sha256 = "0zn8m6r0pwyw6v2kj4ar21q4kzim14m6la7zmx5qz4ikmkmjw7sd";
        }
        {
          # youtube-shorts-block
          id = "jiaopdjbehhjgokpphdfgmapkobbnmjp";
          version = "1.5.3";
          sha256 = "0gs9p5kq62i7yc8k65agpbyay054axswspf0j1bs8hxl0gb43bd7";
        }
        {
          # sponsorblock
          id = "mnjggcdmjocbbbhaepdhchncahnbgone";
          version = "5.9.4";
          sha256 = "0ppi2yg478g15sc0w6ixxbhlvrg6pki93k5vf33dl4cigmw9xhbg";
        }
        {
          # keepassxc
          id = "oboonakemofpalcgghocfoadofidjkkk";
          version = "1.9.3";
          sha256 = "0ajgl28fh6m5p4jl1v60rglxksbi547xv1awh3kp42x510yk4w58";
        }
      ];
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
      banacorn.agda-mode
      llvm-vs-code-extensions.vscode-clangd
      editorconfig.editorconfig
      usernamehw.errorlens
      dbaeumer.vscode-eslint
      tamasfe.even-better-toml
      eamodio.gitlens
      golang.go
      jdinhlife.gruvbox
      haskell.haskell
      justusadam.language-haskell
      james-yu.latex-workshop
      bierner.markdown-mermaid
      pkief.material-icon-theme
      pkief.material-product-icons
      jnoortheen.nix-ide
      christian-kohler.path-intellisense
      esbenp.prettier-vscode
      rust-lang.rust-analyzer
      scalameta.metals
      scala-lang.scala
      timonwong.shellcheck
      vscodevim.vim
      wakatime.vscode-wakatime
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "alloy";
        publisher = "ArashSahebolamri";
        version = "0.7.1";
        sha256 = "svHFOCEDZHSLKzLUU2ojDVkbLTJ7hJ75znWuBV5GFQM=";
      }
      {
        name = "alloy-vscode";
        publisher = "DongyuZhao";
        version = "0.1.6";
        sha256 = "wYMxjMY7colRKWb0qDpMC07+hYhIxh5KcibO43yczPs=";
      }
      {
        name = "bqn-language";
        publisher = "razetime";
        version = "0.1.5";
        sha256 = "50DLHgRE0rozH2XEpyxeHGjtvrSWul5iqUfZfWCTpPw=";
      }
      {
        name = "haskell-gtd-nl";
        publisher = "dbaynak";
        version = "0.3.3";
        sha256 = "Hd7E4NW/zj45xTB/iYvwnPTaFevGAF3EoAnZnEUa6LI=";
      }
      {
        name = "lean4";
        publisher = "leanprover";
        version = "0.0.178";
        sha256 = "ByhiTGwlQgNkFf0BirO+QSDiXbQfR6RLQA8jM4B1+O4=";
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
        name = "markdowntable";
        publisher = "takumii";
        version = "0.11.0";
        sha256 = "kn5aLRaxxacQMvtTp20IdTuiuc6xNU3QO2XbXnzSf7o=";
      }
      {
        name = "paperproof";
        publisher = "paperproof";
        version = "1.1.2";
        sha256 = "NCaQzFa3WQ2kURf4tvrxLykbpPde74cAfhK9olA9m6o=";
      }
      {
        name = "quint-vscode";
        publisher = "informal";
        version = "0.14.5";
        sha256 = "4pWVtLSpb8BpukpvFoBYkGQxQ8hZcmCPQxJGOAGPA2c=";
      }
      {
        name = "vscode-ide";
        publisher = "tlaplus";
        version = "2024.9.132106";
        sha256 = "DLivREQA+vNFZneQ1RjS3mqhG+B/YaDyqm6cbJ37Doc=";
      }
      {
        name = "vsc-prolog";
        publisher = "arthurwang";
        version = "0.8.23";
        sha256 = "Da2dCpruVqzP3g1hH0+TyvvEa1wEwGXgvcmIq9B/2cQ=";
      }
      {
        name = "language-x86-64-assembly";
        publisher = "13xforever";
        version = "3.1.4";
        sha256 = "FJRDm1H3GLBfSKBSFgVspCjByy9m+j9OStlU+/pMfs8=";
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
        (p: with p; [ c go haskell haskell_persistent latex nix rust scala ]))
      vim-wakatime
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
  programs.zathura = {
    enable = true;
    options = {
      font = "SF Mono 10";
      selection-clipboard = "clipboard";
      recolor = false;
      window-title-basename = true;
      statusbar-page-percent = true;
      recolor-darkcolor = "#f3efe6";
      recolor-lightcolor = "#1d2021";
      default-bg = "#212428";
      highlight-color = "#888885";
      statusbar-fg = "#ebdbb2";
      statusbar-bg = "#282828";
      inputbar-fg = "#f3efe6";
      inputbar-bg = "#212428";
      error-color = "#f85f5f";
      warning-color = "#ffb05c";
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
      command = "kgx";
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
