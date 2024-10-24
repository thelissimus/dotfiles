{ pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    plugins = [{
      name = "vi-mode";
      src = pkgs.zsh-vi-mode;
      file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
    }];

    history = {
      extended = true;
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
      theme = "robbyrussell";
    };

    shellAliases = {
      ydl = "yt-dlp -o '%(title)s.%(ext)s' -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best'";
      adl = "yt-dlp -o '%(title)s.%(ext)s' -f 'bestaudio[ext=m4a]/best' --extract-audio";
    };
  };
}
