{ ... }:
{
  programs.nushell = {
    enable = true;

    extraConfig = ''
      $env.config.edit_mode = 'vi'
      $env.config.show_banner = false
    '';

    shellAliases = {
      ydl = "yt-dlp -o '%(title)s.%(ext)s' -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best'";
      adl = "yt-dlp -o '%(title)s.%(ext)s' -f 'bestaudio[ext=m4a]/best' --extract-audio";
    };
  };
}
