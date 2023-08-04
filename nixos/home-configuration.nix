{ config
, pkgs
, ...
}: {
  imports =
    [
      <home-manager/nixos>
    ];

  home-manager.useGlobalPkgs = true;
  home-manager.users.helix = { pkgs, ... }: {
    home.stateVersion = config.system.stateVersion;
    home.file."jdks/jdk8".source = pkgs.jdk8;
    home.file."jdks/jdk17".source = pkgs.jdk17;
    home.file."jdks/scala".source = pkgs.scala;

    programs.bash.enable = true;
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
      keybindings = [
        {
          key = "alt+enter";
          command = "editor.action.quickFix";
          when = "editorHasCodeActionsProvider && textInputFocus && !editorReadonly";
        }
        {
          key = "ctrl+.";
          command = "-editor.action.quickFix";
          when = "editorHasCodeActionsProvider && textInputFocus && !editorReadonly";
        }
        {
          key = "alt+enter";
          command = "-editor.action.selectAllMatches";
          when = "editorFocus && findWidgetVisible";
        }
      ];
      userSettings = builtins.fromJSON (builtins.readFile ../.config/vscode.json);
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
      rnix-lsp
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
