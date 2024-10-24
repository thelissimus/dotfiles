{ pkgs, ... }:
{
  programs.java = {
    enable = true;
    package = pkgs.jdk17;
  };
  home.packages = with pkgs; [
    maven
  ];
}
