{ config
, pkgs
, ...
}: {
  imports =
    [
      ./hardware-configuration.nix
      ./home-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [ "nvidia.NVreg_RegistryDwords=EnableBrightnessControl=1" ];
  boot.supportedFilesystems = [ "ntfs" ];

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;

  time.timeZone = "Etc/GMT-5";
  i18n.defaultLocale = "en_US.UTF-8";

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

  services.openssh.enable = true;
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

  users.users.helix = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
    gnumake
    xclip
    lsof
    strace
    unzip
    fdupes
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };
  hardware.nvidia = {
    modesetting.enable = true;
    nvidiaSettings = true;
    powerManagement.enable = true;
  };

  system.stateVersion = "23.05";
}
