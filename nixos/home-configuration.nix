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
      package = pkgs.vscodium;
      extensions = with pkgs.vscode-extensions; [
        llvm-vs-code-extensions.vscode-clangd
        editorconfig.editorconfig
        usernamehw.errorlens
        dbaeumer.vscode-eslint
        tamasfe.even-better-toml
        golang.go
        jdinhlife.gruvbox
        haskell.haskell
        justusadam.language-haskell
        jnoortheen.nix-ide
        christian-kohler.path-intellisense
        alefragnani.project-manager
        scala-lang.scala
        scalameta.metals
        timonwong.shellcheck
        vscodevim.vim
      ];
      userSettings = {
        "editor.fontFamily" = "'SF Mono', 'Iosevka Semibold'";
        "editor.fontSize" = 17;
        "editor.fontWeight" = "600";
        "editor.inlayHints.enabled" = "off";
        "editor.lineHeight" = 1.6;
        "editor.minimap.enabled" = false;
        "editor.renderWhitespace" = "trailing";
        "editor.tabSize" = 4;
        "explorer.autoReveal" = false;
        "explorer.sortOrder" = "type";
        "files.watcherExclude" = {
          "**/.bloop" = true;
          "**/.metals" = true;
          "**/.ammonite" = true;
        };
        "terminal.integrated.fontSize" = 15;
        "terminal.integrated.fontWeight" = "600";
        "haskell.formattingProvider" = "fourmolu";
        "haskell.manageHLS" = "PATH";
        "window.menuBarVisibility" = "hidden";
        "window.zoomLevel" = 0.5;
        "workbench.colorTheme" = "Gruvbox Dark Medium";
      };
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
        nvim-treesitter.withAllGrammars

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
        lua vim.loader.enable()

        filetype plugin indent on
        syntax on
        language en_US.utf8

        set nocompatible
        set encoding=utf-8
        set fileencoding=utf-8
        set background=dark
        set completeopt=menuone,noinsert,noselect,preview
        set shortmess+=c
        set diffopt+=vertical
        set signcolumn=number
        set colorcolumn=80
        set splitright
        set splitbelow
        set number
        set relativenumber
        set noshowmode
        set cursorline
        set termguicolors
        set nowrap
        set list
        set listchars+=tab:··›,space:·,trail:•
        set noexpandtab
        set tabstop=4
        set shiftwidth=4
        set smartindent
        set ignorecase
        set smartcase
        set incsearch
        set nohlsearch
        set autoread
        set hidden
        set nobackup
        set nowritebackup
        set noswapfile
        set undofile
        set noerrorbells
        set updatetime=50
        set scrolloff=8
        set sidescroll=4
        set laststatus=3
        set pumheight=10
        set spelllang=en_us
        set foldmethod=expr
        set foldexpr=nvim_treesitter#foldexpr()
        set nofoldenable

        let mapleader = " "
        let maplocalleader = " "

        nmap <F5> :set list!<CR>
        nmap <F7> :setlocal spell!<CR>
        nmap <F8> :TagbarToggle<CR>
        nmap <leader>h :bprevious<CR>
        nmap <leader>l :bnext<CR>

        nnoremap <M-j> :resize -2<CR>
        nnoremap <M-k> :resize +2<CR>
        nnoremap <M-h> :vertical resize -5<CR>
        nnoremap <M-l> :vertical resize +5<CR>
        nnoremap <leader>n :NvimTreeToggle<CR>
        nnoremap <leader>u :UndotreeToggle<CR>
        nnoremap <leader><space> :Files<CR>
        nnoremap <leader>ff :Rg<CR>
        nnoremap <leader>bb :Buffers<CR>

        tnoremap <Esc> <C-\><C-n>

        inoremap <Esc> <nop>
        inoremap jk <Esc>
        inoremap kj <Esc>

        let g:EditorConfig_exclude_patterns = ['fugitive://.*']

        let g:gruvbox_material_background = 'medium'
        let g:gruvbox_material_foreground = 'mix'
        let g:gruvbox_material_disable_italic_comment = 1
        let g:gruvbox_material_enable_bold = 1
        let g:gruvbox_material_enable_italic = 0
        let g:gruvbox_material_spell_foreground = 'colored'
        let g:gruvbox_material_lightline_disable_bold = 1
        colorscheme gruvbox-material

        let g:lightline = {
          \ 'enable': { 'statusline': 1, 'tabline': 0 },
          \ 'colorscheme': 'gruvbox_material',
          \ 'active': {
          \ 	'left': [
          \ 		['mode', 'paste'],
          \ 		['gitbranch', 'readonly', 'filename'],
          \		['lsp_info', 'lsp_hints', 'lsp_errors', 'lsp_warnings', 'lsp_ok'],
          \		['lsp_status']
          \ 	],
          \ },
          \ 'component_function': {
          \ 	'gitbranch': 'FugitiveHead',
          \ 	'filename': 'LightlineFilename'
          \ },
          \ }
        call lightline#lsp#register()

        function! LightlineFilename()
          let filename = expand('%:t') !=# ''' ? expand('%:t') : '[Buffer]'
          let modified = &modified ? ' +' : '''
          return filename . modified
        endfunction

        autocmd! FileType fzf tnoremap <buffer> <esc> <c-c>
        autocmd BufNewFile,BufRead *.service set ft=systemd
        autocmd BufNewFile,BufRead * setlocal formatoptions-=o formatoptions+=r

        lua << EOF
        local capabilities = require("cmp_nvim_lsp").default_capabilities()
        local lspconfig = require('lspconfig')
        local cmp = require('cmp')

        vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
        vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
        vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
        vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

        vim.api.nvim_create_autocmd('LspAttach', {
          group = vim.api.nvim_create_augroup('UserLspConfig', {}),
          callback = function(ev)
            local opts = { buffer = ev.buf }
            vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
            vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
            vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
            vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
            vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
            vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
            vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
            vim.keymap.set('n', '<space>wl', function()
              print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
            end, opts)
            vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
            vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
            vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
            vim.keymap.set('n', '<space>f', function()
              vim.lsp.buf.format { async = true }
            end, opts)
          end,
        })

        local servers = { 'metals', 'rnix', 'rust_analyzer', 'vhdl_ls' }
        for _, lsp in ipairs(servers) do
          lspconfig[lsp].setup {
            capabilities = capabilities,
          }
        end

        lspconfig['hls'].setup {
          filetypes = { 'haskell', 'lhaskell', 'cabal' },
          settings = {
            haskell = {
              cabalFormattingProvider = "cabalfmt",
              formattingProvider = "fourmolu",
            }
          }
        }

        cmp.setup {
          snippet = {
            expand = function(args)
              vim.fn["vsnip#anonymous"](args.body)
            end,
          },
          window = {
            completion = cmp.config.window.bordered(),
            documentation = cmp.config.window.bordered(),
          },
          mapping = cmp.mapping.preset.insert({
            ['<C-b>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-Space>'] = cmp.mapping.complete(),
            ['<C-e>'] = cmp.mapping.abort(),
            ['<CR>'] = cmp.mapping.confirm({ select = true }),
          }),
          sources = cmp.config.sources({
            { name = 'nvim_lsp' },
            { name = 'vsnip' },
            { name = 'path' },
          }, {
            { name = 'buffer' },
          })
        }

        cmp.setup.cmdline({ ':', '/', '?' }, {
          mapping = cmp.mapping.preset.cmdline(),
          sources = cmp.config.sources({
            { name = 'path' }
          }, {
            { name = 'cmdline' }
          })
        })

        vim.diagnostic.config({
          virtual_text = true,
          signs = true,
          underline = true,
          update_in_insert = true,
          severity_sort = false,
        })

        require("nvim-treesitter.configs").setup({
          auto_install = false,
          highlight = {
            enable = true,
            additional_vim_regex_highlighting = false,
          },
          indent = {
            enable = false,
          }
        })
        require("nvim-tree").setup()
        EOF
      '';
    };

    home.packages = with pkgs; [
      anki
      firefox
      librewolf
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
      tokei
      tree
      nicotine-plus
      htop
      nvtop
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
      # Media
      ffmpeg
      mpv
      vlc
      yt-dlp
      obs-studio
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
      # Java
      maven
      # jdk8
      jdk17
      # Nix
      rnix-lsp
      nixpkgs-fmt
      # Node.js
      nodejs_20
      nodePackages.pnpm
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
