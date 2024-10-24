{ ... }:
{
  programs.wezterm = {
    enable = true;
    extraConfig = builtins.readFile ../../.config/wezterm/wezterm.lua;
  };
}
