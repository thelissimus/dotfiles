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
