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

    brews = [ ];
    casks = [
      "nikitabobko/tap/aerospace"
      "alloy"
      "alt-tab"
      "amethyst"
      "anki"
      "betterdisplay"
      "claude"
      "deepl"
      "discord"
      "figma"
      "firefox"
      "flameshot"
      "font-iosevka"
      "font-mononoki"
      "font-sf-mono"
      "font-symbols-only-nerd-font"
      "ghostty"
      "gitbutler"
      "iina"
      "iterm2"
      "keepassxc"
      "keka"
      "linear-linear"
      "lulu"
      "macfuse"
      "mullvad-vpn"
      "obs"
      "obsidian"
      "orbstack"
      "prismlauncher"
      "qbittorrent"
      "simplex"
      "sioyek"
      "slack"
      "smoothscroll"
      "steam"
      "telegram-desktop"
      "ticktick"
      "ungoogled-chromium"
      "unnaturalscrollwheels"
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
  system = {
    inherit configurationRevision;
    stateVersion = 5;
    primaryUser = username;
  };
  nixpkgs.hostPlatform = system;
  users.users.${username} = {
    name = username;
    home = "/Users/${username}";
  };
}
