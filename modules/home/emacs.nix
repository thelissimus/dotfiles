{ pkgs, config, ... }:
{
  programs.zsh.shellAliases.eapp = "open ${config.programs.emacs.finalPackage}/Applications/Emacs.app";
  programs.nushell.shellAliases.eapp = "open ${config.programs.emacs.finalPackage}/Applications/Emacs.app";

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-macport;
    extraConfig = builtins.readFile ../../.config/emacs/init.el;
    extraPackages = epkgs: with epkgs; [
      dashboard
      projectile
      perspective
      persp-projectile
      nerd-icons
      nerd-icons-dired
      doom-modeline
      olivetti

      highlight-indent-guides
      hl-todo

      envrc
      super-save
      ws-butler
      evil
      evil-collection
      evil-multiedit
      gruvbox-theme
      magit
      diff-hl
      difftastic
      dired-git-info
      vundo
      winum
      wakatime-mode

      vertico
      orderless
      marginalia
      consult
      consult-todo
      consult-hoogle
      embark
      embark-consult
      wgrep

      eglot-booster
      eldoc-box
      company
      company-coq
      nix-ts-mode
      haskell-ts-mode
      proof-general
      just-ts-mode
      (treesit-grammars.with-grammars (p: with p; [
        tree-sitter-nix
        tree-sitter-haskell
        tree-sitter-elisp

        tree-sitter-bash
        tree-sitter-json
        tree-sitter-just
        tree-sitter-yaml
        tree-sitter-toml
      ]))

      paredit
      enhanced-evil-paredit

      vterm
      vterm-toggle
    ];
  };
}
