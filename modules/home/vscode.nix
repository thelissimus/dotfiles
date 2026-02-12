{ pkgs, ... }:
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
    profiles.default = {
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = false;
      extensions = with pkgs.vscode-extensions; [
        svelte.svelte-vscode
        banacorn.agda-mode
        llvm-vs-code-extensions.vscode-clangd
        editorconfig.editorconfig
        usernamehw.errorlens
        dbaeumer.vscode-eslint
        tamasfe.even-better-toml
        eamodio.gitlens
        jdinhlife.gruvbox
        haskell.haskell
        ms-python.python
        ms-toolsai.jupyter
        justusadam.language-haskell
        james-yu.latex-workshop
        leanprover.lean4
        sumneko.lua
        bierner.markdown-mermaid
        bierner.markdown-checkbox
        bierner.markdown-preview-github-styles
        pkief.material-icon-theme
        pkief.material-product-icons
        jnoortheen.nix-ide
        christian-kohler.path-intellisense
        esbenp.prettier-vscode
        rust-lang.rust-analyzer
        timonwong.shellcheck
        vscodevim.vim
        wakatime.vscode-wakatime
        ziglang.vscode-zig
        ocamllabs.ocaml-platform
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
          name = "markdowntable";
          publisher = "takumii";
          version = "0.11.0";
          sha256 = "kn5aLRaxxacQMvtTp20IdTuiuc6xNU3QO2XbXnzSf7o=";
        }
        {
          name = "language-x86-64-assembly";
          publisher = "13xforever";
          version = "3.1.4";
          sha256 = "FJRDm1H3GLBfSKBSFgVspCjByy9m+j9OStlU+/pMfs8=";
        }
        {
          name = "coq-lsp";
          publisher = "ejgallego";
          version = "0.2.4";
          sha256 = "s2f2i3sNZ3EdCHDgkYPPiXDp25cViAZy+DpnDxfWaSo=";
        }
        {
          name = "wasm-wasi-core";
          publisher = "ms-vscode";
          version = "1.0.2";
          sha256 = "hrzPNPaG8LPNMJq/0uyOS8jfER1Q0CyFlwR42KmTz8g=";
        }
        {
          name = "claude-code";
          publisher = "anthropic";
          version = "2.0.65";
          sha256 = "nHZCEEWEgBdxAzpLFkQsTwNPx3JxuwhgwxKgW8LJ450=";
        }
        {
          name = "specforge";
          publisher = "imiron";
          version = "0.5.5";
          sha256 = "yJrzOBQbNitQMbdMxWwO/VFMk3Bh4azwZEnPiIEiQr0=";
        }
        {
          name = "slipshow";
          publisher = "Slipshow";
          version = "0.0.7";
          sha256 = "Q0/EbKFgtO29F0PmLQjDQOyEID2lsiOwET4i3lUmRO8=";
        }
      ];
      keybindings = builtins.fromJSON (builtins.readFile ../../.config/Code/User/keybindings.json);
      userSettings = builtins.fromJSON (builtins.readFile ../../.config/Code/User/settings.json);
    };
  };
}
