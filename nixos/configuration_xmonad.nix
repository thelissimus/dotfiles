{ config, pkgs, ... }:

{
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true;
  };

  services.autorandr.enable = true;

  services.xserver = {
    autorun = true;
    xrandrHeads = [
      { output = "HDMI-0"; primary = true; }
      { output = "DP-4"; }
    ];
    libinput = {
      enable = true;
      touchpad = {
        naturalScrolling = false;
      };
    };

    desktopManager.xterm.enable = true;
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    displayManager = {
      defaultSession = "none+xmonad";
      sessionCommands = ''
        xrandr --output DP-4 --off --output HDMI-0 --mode 1920x1080 --primary --right-of DP-4 --rate 75 --dpi 82
        trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x5f5f5f --height 18 &
        feh --bg-fill --no-fehbg ~/Pictures/wallpapers/space_flower.jpg
        xscreensaver -no-splash &
      '';
    };
  };

  sound.mediaKeys.enable = true;
  hardware.bluetooth.enable = true;

  system.copySystemConfiguration = true;
}
