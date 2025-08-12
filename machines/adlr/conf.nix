{ pkgs, configurationRevision, ... }: {
  environment.systemPackages =
    [
      pkgs.vim
    ];
  homebrew = {
    enable = true;

    taps = [ ];
    brews = [ ];
    casks = [
      "alloy"
      "alt-tab"
      "anki"
      "betterdisplay"
      "chatgpt"
      "deepl"
      "discord"
      "figma"
      "firefox"
      "flameshot"
      "font-iosevka"
      "font-mononoki"
      "font-sf-mono"
      "ghostty"
      "gitbutler"
      "iina"
      "iterm2"
      "keepassxc"
      "keka"
      "linear-linear"
      "macfuse"
      "obs"
      "obsidian"
      "orbstack"
      "qbittorrent"
      "sioyek"
      "steam"
      "slack"
      "telegram"
      "ticktick"
      "ungoogled-chromium"
      "unnaturalscrollwheels"
      "visual-studio-code"
      "vlc"
      "wezterm"
      "zed"
      "zen-browser"
      "zoom"
      "zulip"
    ];
  };

  nix.enable = false;
  programs.zsh.enable = true;
  system.configurationRevision = configurationRevision;
  system.stateVersion = 5;
  system.primaryUser = "kei";
  nixpkgs.hostPlatform = "aarch64-darwin";
  users.users.kei = {
    name = "kei";
    home = "/Users/kei";
  };
}
