{ pkgs, hostname, username, ... }:
{
  imports =
    [
      ./hardware.nix
    ];

  boot = {
    loader.grub.enable = true;
    loader.grub.device = "/dev/sda";
    swraid.enable = false;
  };

  networking.hostName = hostname;
  networking.networkmanager.enable = true;

  time.timeZone = "Etc/GMT-5";
  i18n.defaultLocale = "en_US.UTF-8";

  services = {
    xserver = {
      enable = true;
      xkb = {
        layout = "us,ru";
        options = "caps:escape";
      };
      displayManager = {
        lightdm.enable = true;
      };
      desktopManager.xfce.enable = true;
    };

    displayManager.defaultSession = "xfce";
    libinput.enable = true;
    pulseaudio.enable = false;
  };

  users.defaultUserShell = pkgs.zsh;
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
    neovim
    wget
    gnumake
    xclip
    zip
    unzip
    git
  ];

  programs.zsh.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.openssh.enable = true;

  system.stateVersion = "23.11";
}
