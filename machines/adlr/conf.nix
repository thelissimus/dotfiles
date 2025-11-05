{ pkgs, system, username, configurationRevision, ... }: {
  environment.systemPackages = with pkgs; [
    vim
    radicle-node
    radicle-httpd
  ];

  homebrew = {
    enable = true;

    onActivation = {
      cleanup = "zap";
    };

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
      "mullvad-vpn"
      "obs"
      "obsidian"
      "orbstack"
      "prismlauncher"
      "qbittorrent"
      "sioyek"
      "slack"
      "steam"
      "telegram-desktop"
      "ticktick"
      "ungoogled-chromium"
      "unnaturalscrollwheels"
      "visual-studio-code"
      "vlc"
      "wezterm"
      "zed"
      "zen"
      "zoom"
      "zotero"
      "zulip"
    ];
  };

  nix.enable = false;
  programs.zsh.enable = true;
  system.configurationRevision = configurationRevision;
  system.stateVersion = 5;
  system.primaryUser = username;
  nixpkgs.hostPlatform = system;
  users.users.${username} = {
    name = username;
    home = "/Users/${username}";
  };
}
