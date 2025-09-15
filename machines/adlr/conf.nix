{ pkgs, configurationRevision, ... }: {
  environment.systemPackages = with pkgs; [
    vim
    time
    radicle-node
    radicle-httpd
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
      "telegram-desktop"
      "ticktick"
      "ungoogled-chromium"
      "unnaturalscrollwheels"
      "visual-studio-code"
      "vlc"
      "wezterm"
      "zed"
      "zen-browser"
      "zotero"
      "zoom"
      "zulip"
    ];
  };

  nix.enable = false;
  programs.zsh.enable = true;
  system = {
    inherit configurationRevision;
    stateVersion = 5;
    primaryUser = "kei";
  };
  nixpkgs.hostPlatform = "aarch64-darwin";
  users.users.kei = {
    name = "kei";
    home = "/Users/kei";
  };
}
