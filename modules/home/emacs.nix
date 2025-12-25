{ pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-macport;
    extraConfig = ''
      (setq inhibit-startup-message t)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)
      (set-face-attribute 'default nil :family "Mononoki" :height 150 :weight 'semibold)
      (setq-default line-spacing 0.3)
      (global-display-line-numbers-mode t)
      (setq display-line-numbers-type 'relative)

      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      (require 'evil)
      (evil-mode 1)
      (evil-collection-init)

      (load-theme 'gruvbox-dark-medium t)
    '';
    extraPackages = epkgs: with epkgs; [
      evil
      evil-collection
      gruvbox-theme
      magit
    ];
  };
}
