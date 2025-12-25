{ pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-macport;
    extraConfig = ''
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
