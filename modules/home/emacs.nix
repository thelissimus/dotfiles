{ pkgs, config, ... }:
{
  programs.zsh.shellAliases.eapp = "open ${config.programs.emacs.finalPackage}/Applications/Emacs.app";

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
      nix-ts-mode
      haskell-ts-mode
      just-ts-mode
      (treesit-grammars.with-grammars (p: with p; [
        tree-sitter-nix
        tree-sitter-haskell
        tree-sitter-elisp

        tree-sitter-bash
        tree-sitter-json
        tree-sitter-just
        tree-sitter-yaml
      ]))

      paredit
      enhanced-evil-paredit

      vterm
      vterm-toggle
    ];
  };
}
