{ ... }:
{
  programs.git = {
    enable = true;
    lfs.enable = true;

    ignores = [
      ".DS_Store"

      "*.eld"
      "*~"
      "\\#*\\#"
      ".dir-locals.el"

      "**/.claude/settings.local.json"
      "CLAUDE.md"
    ];

    settings = {
      core.editor = "nvim";
      user.name = "thelissimus";
      user.email = "thelissimus@tuta.io";
      init.defaultBranch = "master";
      rerere.enabled = true;
      diff.algorithm = "histogram";
      merge.conflictStyle = "zdiff3";
    };

    signing = {
      key = "464CA2FF1D27D92C";
      signByDefault = true;
    };
  };

  programs.difftastic = {
    enable = true;
    git.enable = true;
  };
}
