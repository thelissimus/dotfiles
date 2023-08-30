{ pkgs, ... }: {
  imports =
    [
      ./hardware-configuration.nix
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [
    "nvidia.NVreg_RegistryDwords=EnableBrightnessControl=1"
  ];
  boot.supportedFilesystems = [ "ntfs" ];

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;

  time.timeZone = "Etc/GMT-5";
  i18n.defaultLocale = "de_DE.UTF-8";
  i18n.extraLocaleSettings = {
    LANGUAGE = "de_DE";
    LC_ALL = "de_DE.UTF-8";
  };

  services.xserver = {
    enable = true;
    layout = "us,ru,de";
    xkbOptions = "caps:escape,grp:alt_shift_toggle";
    xkbVariant = "altgr-intl";
    displayManager = {
      gdm.enable = true;
      gdm.wayland = false;
    };
    desktopManager = {
      gnome.enable = true;
    };
    videoDrivers = [ "nvidia" ];
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_15;
    ensureDatabases = [ "helix" ];
    ensureUsers = [
      {
        name = "helix";
        ensurePermissions = {
          "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
        };
      }
    ];
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all              trust
      host  all all 127.0.0.1/32 trust
      host  all all ::1/128      trust
    '';
  };

  services.openssh.enable = true;
  services.pcscd.enable = true;
  services.printing.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.defaultUserShell = pkgs.zsh;
  users.users.helix = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "docker" ];
  };

  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };
  environment.shells = with pkgs; [ zsh ];
  environment.systemPackages = with pkgs; [
    file
    vim
    wget
    gnumake
    xclip
    lsof
    strace
    zip
    unzip
    fdupes
  ];
  environment.gnome.excludePackages = (with pkgs; [
    gnome-photos
    gnome-tour
  ]) ++ (with pkgs.gnome; [
    cheese
    gnome-music
    gnome-terminal
    gedit
    epiphany
    geary
    evince
    totem
    tali
    iagno
    hitori
    atomix
  ]);

  fonts = {
    fonts = with pkgs; [
      noto-fonts-cjk
    ];
    fontconfig = {
      enable = true;
      localConf = builtins.readFile ../.config/fontconfig/fonts.conf;
    };
  };

  programs.zsh.enable = true;
  programs.autojump.enable = true;
  programs.dconf.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "tty";
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
  };
  hardware.nvidia = {
    nvidiaSettings = true;
    powerManagement.enable = true;
  };

  system.stateVersion = "23.05";
}
