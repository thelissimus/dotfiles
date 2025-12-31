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
      nerd-icons
      doom-modeline
      treemacs
      treemacs-evil
      treemacs-projectile
      treemacs-nerd-icons

      envrc
      evil
      evil-collection
      gruvbox-theme
      magit
      winum

      vertico
      orderless
      marginalia
      consult

      eldoc-box
      company
      nix-mode
      haskell-mode

      vterm
      vterm-toggle
    ];
  };
}
