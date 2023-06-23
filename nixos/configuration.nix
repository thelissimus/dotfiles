{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = ["nvidia.NVreg_RegistryDwords=EnableBrightnessControl=1"];

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
    videoDrivers = ["nvidia"];
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
    description = "helix";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      firefox
      telegram-desktop
      keepassxc
      qbittorrent
      baobab
      gcolor3
      flameshot
      qalculate-gtk
      rename
      rlwrap
      ripgrep
      fzf
      tokei
      tree
      nicotine-plus
      nvtop
      pfetch
      sakura
      insomnia
      pinentry
      git

      gnomeExtensions.dash-to-dock
      gnomeExtensions.appindicator

      # Editor
      vscodium
      (vscode-with-extensions.override {
        vscode = vscodium;
        vscodeExtensions = with vscode-extensions; [
          llvm-vs-code-extensions.vscode-clangd
          editorconfig.editorconfig
          usernamehw.errorlens
          dbaeumer.vscode-eslint
          tamasfe.even-better-toml
          jdinhlife.gruvbox
          golang.go
          haskell.haskell
          pkief.material-product-icons
          christian-kohler.path-intellisense
          alefragnani.project-manager
          timonwong.shellcheck
          vscodevim.vim
          jnoortheen.nix-ide
        ];
      })

      # Documents
      doxygen
      obsidian
      zathura

      # Media
      mpv
      vlc
      obs-studio
      figma-agent
      figma-linux
      feh
      optipng
      peek

      # Database
      postgresql_15

      # Message Queue
      rabbitmq-server

      # Tools
      android-tools
      docker

      # C
      clang_16
      llvmPackages_16.clang-unwrapped
      gcc
      gdb
      tinycc

      # Clojure
      clojure
      leiningen

      # Go
      go

      # Haskell
      ghc
      ghcid
      haskell-language-server
      cabal-install
      stack

      # Java
      maven
      jdk8
      jdk17

      # Node.js
      nodejs_20
      nodePackages.npm
      nodePackages.pnpm

      # Scala
      dotty
      scala
      sbt
      scalafmt
      scalafix
      scala-cli
    ];
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
    gnumake
    neovim
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
