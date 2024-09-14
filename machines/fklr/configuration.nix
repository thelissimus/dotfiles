{ pkgs, hostname, username, ... }:
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.swraid.enable = false;

  networking.hostName = hostname;
  networking.networkmanager.enable = true;

  time.timeZone = "Etc/GMT-5";
  i18n.defaultLocale = "en_US.UTF-8";

  services.xserver = {
    enable = true;
    xkb = {
      layout = "us,ru";
      options = "caps:escape";
    };
    desktopManager.xfce.enable = true;
    displayManager = {
      lightdm.enable = true;
      defaultSession = "xfce";
    };
    libinput.enable = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users.${username} = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      firefox
    ];
  };

  environment.systemPackages = with pkgs; [
    file
    vim
    wget
    gnumake
    xclip
    zip
    unzip
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.openssh.enable = true;

  system.stateVersion = "23.11";
}
