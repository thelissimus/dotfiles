{ ... }:
{
  programs.direnv = {
    enable = true;
    enableNushellIntegration = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
}
