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

  programs.zsh.shellAliases.emacs = "open ${emacsPkg}/Applications/Emacs.app";

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-macport;
    extraConfig = ''
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (set-face-attribute 'default nil :family "Mononoki" :height 170 :weight 'semi-bold)
      (setq-default line-spacing 0.2)
      (global-display-line-numbers-mode t)
      (setq display-line-numbers-type 'relative)

      (require 'projectile)
      (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
      (projectile-mode +1)

      (require 'nerd-icons)

      (require 'dashboard)
      (setq dashboard-banner-logo-title "")
      (setq dashboard-startup-banner 1)
      (setq dashboard-center-content t)
      (setq dashboard-vertically-center-content t)
      (setq dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
      (setq dashboard-projects-backend 'projectile)
      (setq dashboard-display-icons-p t)
      (setq dashboard-icon-type 'nerd-icons)
      (setq dashboard-set-heading-icons t)
      (setq dashboard-set-file-icons t)
      (dashboard-setup-startup-hook)

      (require 'envrc)
      (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
      (envrc-global-mode)

      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      (require 'evil)
      (evil-mode 1)
      (evil-collection-init)

      (load-theme 'gruvbox-dark-medium t)
    '';
    extraPackages = epkgs: with epkgs; [
      dashboard
      projectile
      nerd-icons

      envrc
      evil
      evil-collection
      gruvbox-theme
      magit
    ];
  };
}
