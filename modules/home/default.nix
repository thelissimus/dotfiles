{ pkgs, lib, ... }:
{
  bash = import ./bash.nix { inherit pkgs; };
  chromium = import ./chromium.nix { inherit pkgs lib; };
  direnv = import ./direnv.nix { inherit pkgs; };
  firefox = import ./firefox.nix { inherit pkgs; };
  git = import ./git.nix { inherit pkgs; };
  java = import ./java.nix { inherit pkgs; };
  neovim = import ./neovim.nix { inherit pkgs; };
  vscode = import ./vscode.nix { inherit pkgs; };
  wezterm = import ./wezterm.nix { inherit pkgs; };
  zathura = import ./zathura.nix { inherit pkgs; };
  zoxide = import ./zoxide.nix { inherit pkgs; };
  zsh = import ./zsh.nix { inherit pkgs; };
}
