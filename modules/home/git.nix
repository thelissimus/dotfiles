{ ... }:
{
  programs.git = {
    enable = true;
    lfs.enable = true;
  };
  programs.difftastic = {
    enable = true;
    git.enable = true;
  };
}
