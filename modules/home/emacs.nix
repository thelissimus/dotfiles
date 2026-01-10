{ pkgs, config, ... }:
let
  emacsPkg = config.programs.emacs.finalPackage;
in
{
  home.activation.linkEmacsApp = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    app_src="${emacsPkg}/Applications/Emacs.app"
    app_dst="$HOME/Applications/Nix Apps/Emacs.app"
    mkdir -p "$HOME/Applications/Nix Apps"
    rm -rf "$app_dst"
    ${pkgs.mkalias}/bin/mkalias "$app_src" "$app_dst"
  '';

  programs.zsh.shellAliases.emacsapp = "open ${emacsPkg}/Applications/Emacs.app";

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
      doom-modeline
      treemacs
      treemacs-evil
      treemacs-projectile
      treemacs-magit
      treemacs-perspective
      treemacs-nerd-icons

      highlight-indent-guides
      hl-todo

      envrc
      super-save
      evil
      evil-collection
      gruvbox-theme
      magit
      diff-hl
      vundo
      winum
      wakatime-mode

      vertico
      orderless
      marginalia
      consult
      consult-todo
      embark
      embark-consult
      wgrep

      eglot-booster
      eldoc-box
      company
      nix-mode
      haskell-mode

      paredit
      enhanced-evil-paredit

      vterm
      vterm-toggle
    ];
  };
}
