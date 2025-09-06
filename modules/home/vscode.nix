{ pkgs, ... }:
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
    profiles.default = {
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = false;
      extensions = (with pkgs.vscode-extensions; [
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
        justusadam.language-haskell
        james-yu.latex-workshop
        bierner.markdown-mermaid
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
          name = "language-x86-64-assembly";
          publisher = "13xforever";
          version = "3.1.4";
          sha256 = "FJRDm1H3GLBfSKBSFgVspCjByy9m+j9OStlU+/pMfs8=";
        }
        {
          name = "vscoq";
          publisher = "maximedenes";
          version = "2.1.7";
          sha256 = "H3/mXHmtuLknG1k74nFcp+E2VnJ286R4Rlo8GdHb4ag=";
        }
      ]);
      keybindings = builtins.fromJSON (builtins.readFile ../../.config/Code/User/keybindings.json);
      userSettings = builtins.fromJSON (builtins.readFile ../../.config/Code/User/settings.json);
    };
  };
}
