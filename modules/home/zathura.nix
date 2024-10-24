{ ... }:
{
  programs.zathura = {
    enable = true;
    options = {
      font = "SF Mono 10";
      selection-clipboard = "clipboard";
      recolor = false;
      window-title-basename = true;
      statusbar-page-percent = true;
      recolor-darkcolor = "#ebebec";
      recolor-lightcolor = "#161618";
      default-bg = "#1a1a1b";
      highlight-color = "#757579";
      statusbar-fg = "#d5d5d6";
      statusbar-bg = "#202123";
      inputbar-fg = "#d0cec9";
      inputbar-bg = "#17181a";
      error-color = "#e06c75";
      warning-color = "#d19a66";
    };
  };
}
