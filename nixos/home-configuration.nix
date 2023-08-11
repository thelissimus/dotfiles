{ config
, ...
}: {
  imports =
    [
      <home-manager/nixos>
    ];

  home-manager.useGlobalPkgs = true;
  home-manager.users.helix = { pkgs, lib, ... }:
    let
      inherit (lib.hm.gvariant) mkTuple mkUint32;
    in
    {
      home.stateVersion = config.system.stateVersion;
      home.file."jdks/jdk8".source = pkgs.jdk8;
      home.file."jdks/jdk17".source = pkgs.jdk17;
      home.file."jdks/scala".source = pkgs.scala;

      programs.bash.enable = true;
      programs.zsh.enable = true;
      programs.vscode = {
        enable = true;
        enableExtensionUpdateCheck = false;
        enableUpdateCheck = false;
        package = pkgs.vscode-fhs;
        extensions = with pkgs.vscode-extensions; [
          llvm-vs-code-extensions.vscode-clangd
          dhall.vscode-dhall-lsp-server
          editorconfig.editorconfig
          usernamehw.errorlens
          dbaeumer.vscode-eslint
          tamasfe.even-better-toml
          donjayamanne.githistory
          golang.go
          jdinhlife.gruvbox
          haskell.haskell
          justusadam.language-haskell
          sumneko.lua
          pkief.material-icon-theme
          pkief.material-product-icons
          jnoortheen.nix-ide
          christian-kohler.path-intellisense
          esbenp.prettier-vscode
          alefragnani.project-manager
          # nwolverson.ide-purescript
          # nwolverson.language-purescript
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
        };
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
          font-name = "SF Pro Display 11";
          document-font-name = "SF Pro Display 11";
          monospace-font-name = "SF Mono Medium 12";
          show-battery-percentage = true;
        };
        "org/gnome/desktop/peripherals/touchpad" = {
          two-finger-scrolling-enabled = true;
        };
        "org/gnome/desktop/session" = {
          idle-delay = 900;
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
          sleep-inactive-battery-timeout = 180;
          sleep-inactive-battery-type = "nothing";
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
      };

      home.packages = with pkgs; [
        anki
        firefox
        librewolf
        telegram-desktop
        discord
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
        sakura
        insomnia
        pinentry
        difftastic
        git
        gnome.gnome-tweaks
        gnomeExtensions.dash-to-dock
        gnomeExtensions.appindicator
        # Editor
        jetbrains.idea-community
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
        postgresql_15
        sqlite
        # Message Queue
        rabbitmq-server
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
        ghcid
        haskell-language-server
        cabal-install
        stack
        haskellPackages.cabal-fmt
        haskellPackages.fourmolu
        haskellPackages.hpack
        haskellPackages.hlint
        haskellPackages.implicit-hie
        haskellPackages.stylish-haskell
        # Java
        maven
        # jdk8
        jdk17
        # Nix
        nil
        nixpkgs-fmt
        # JS
        deno
        nodejs_20
        nodePackages.pnpm
        # Purescript
        purescript
        spago
        nodePackages.purescript-language-server
        dhall-lsp-server
        # Rust
        rustup
        # Scala
        metals
        dotty
        sbt
        scalafmt
        scalafix
        scala-cli
        # VHDL
        ghdl
        vhdl-ls
      ];
    };
}
