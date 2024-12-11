{ pkgs, ... }:
{
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
      cornelis
      vim-which-key
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
}
