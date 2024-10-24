{ pkgs, hostname, username, ... }: {
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [
    "nvidia.NVreg_RegistryDwords=EnableBrightnessControl=1"
  ];
  boot.supportedFilesystems = [ "ntfs" ];
  boot.swraid.enable = false;

  networking.hostName = hostname;
  networking.extraHosts =
    ''
      127.0.0.1 gemini.google.com
    '';
  networking.networkmanager.enable = true;

  time.timeZone = "Etc/GMT-5";
  i18n.defaultLocale = "de_DE.UTF-8";
  i18n.extraLocaleSettings = {
    LANGUAGE = "de_DE";
    LC_ALL = "de_DE.UTF-8";
  };

  services.xserver = {
    enable = true;
    xkb = {
      layout = "us,ru,de";
      options = "caps:escape,grp:alt_shift_toggle";
      variant = "altgr-intl";
    };
    displayManager = {
      gdm.enable = true;
      gdm.wayland = false;
    };
    desktopManager = {
      gnome.enable = true;
    };
    videoDrivers = [ "nvidia" ];
  };

  services.openssh.enable = true;
  services.pcscd.enable = true;
  services.printing.enable = true;
  services.earlyoom.enable = true;
  services.earlyoom.freeMemThreshold = 5;
  services.thermald.enable = true;

  services.redis.servers."global".enable = true;
  services.redis.servers."global".port = 6379;
  services.redis.vmOverCommit = true;

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.defaultUserShell = pkgs.zsh;
  users.users.${username} = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "docker" ];
  };

  virtualisation.docker.enable = true;
  environment.shells = with pkgs; [ zsh ];
  environment.systemPackages = with pkgs; [
    docker-compose
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
    libGL
    pulseaudio
  ];
  environment.gnome.excludePackages = (with pkgs; [
    gnome-photos
    gnome-tour
    gedit
    cheese
    gnome-music
    gnome-console
    gnome-terminal
    epiphany
    geary
    evince
    totem
    tali
    iagno
    hitori
    atomix
    seahorse
  ]);
  environment.variables = {
    LD_LIBRARY_PATH = "$LD_LIBRARY_PATH:${pkgs.libGL}/lib";
  };

  fonts = {
    packages = with pkgs; [
      noto-fonts-cjk
      iosevka-bin
      julia-mono
      apple-fonts.sf-pro
      apple-fonts.sf-mono
      apple-fonts.ny
    ];
    fontconfig = {
      enable = true;
      localConf = builtins.readFile ../../.config/fontconfig/fonts.conf;
    };
  };

  programs.steam.enable = true;
  programs.zsh.enable = true;
  programs.ssh.extraConfig = ''
    Host *
    ServerAliveInterval 120
  '';
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-curses;
  };

  hardware.graphics = {
    enable = true;
  };
  hardware.nvidia = {
    open = false;
    modesetting.enable = true;
    nvidiaSettings = false;
    powerManagement.enable = true;
  };
  hardware.bluetooth.settings = {
    General = {
      Experimental = true;
    };
  };

  system.stateVersion = "23.05";
}
