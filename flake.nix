{
  description = "flakes for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    k.url = "github:runtimeverification/k";
    k.inputs.nixpkgs.follows = "nixpkgs";
    apple-fonts.url = "path:pkgs/apple-fonts";
    apple-fonts.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, nix-darwin, nix-homebrew, home-manager, apple-fonts, nur, k, ... }:
    let
      mkPkgs = system: import nixpkgs {
        localSystem = { inherit system; };
        config = {
          permittedInsecurePackages = [
            "electron-32.3.3"
          ];
          allowUnfree = true;
        };
        overlays = [
          (final: prev: {
            apple-fonts = apple-fonts.packages.${system};
          })
          k.overlay
          nur.overlays.default
        ];
      };
      mkSystem = { system, hostname, username, modules ? [ ] }: nixpkgs.lib.nixosSystem {
        inherit system;
        pkgs = mkPkgs system;
        specialArgs = { inherit inputs hostname username; };
        modules = [
          ./machines/${hostname}/configuration.nix
          ./modules/core/nix.nix
        ] ++ modules;
      };
    in
    {
      formatter.x86_64-linux = (mkPkgs "x86_64-linux").nixpkgs-fmt;
      formatter.aarch64-darwin = (mkPkgs "aarch64-darwin").nixpkgs-fmt;

      nixosConfigurations = {
        vega = let username = "helix"; in mkSystem {
          system = "x86_64-linux";
          inherit username;
          hostname = "vega";
          modules = [
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${username} = import ./machines/vega/home-configuration.nix;
            }
            (import ./modules/services/postgres.nix {
              inherit (mkPkgs "x86_64-linux") pkgs;
              inherit username;
            })
          ];
        };
        fklr = mkSystem { system = "x86_64-linux"; hostname = "fklr"; username = "alice"; };
      };

      darwinConfigurations."Keis-MacBook-Pro" = nix-darwin.lib.darwinSystem {
        pkgs = mkPkgs "aarch64-darwin";
        modules =
          let
            configuration = { pkgs, ... }: {
              environment.systemPackages =
                [
                  pkgs.vim
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
                  "telegram"
                  "ticktick"
                  "ungoogled-chromium"
                  "unnaturalscrollwheels"
                  "visual-studio-code"
                  "vlc"
                  "wezterm"
                  "zed"
                  "zen-browser"
                  "zoom"
                  "zulip"
                ];
              };

              nix.enable = false;
              programs.zsh.enable = true;
              system.configurationRevision = self.rev or self.dirtyRev or null;
              system.stateVersion = 5;
              system.primaryUser = "kei";
              nixpkgs.hostPlatform = "aarch64-darwin";
              users.users.kei = {
                name = "kei";
                home = "/Users/kei";
              };
            };
          in
          [
            configuration
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users."kei" = import ./machines/adlr/home.nix;
            }
            nix-homebrew.darwinModules.nix-homebrew
            {
              nix-homebrew = {
                enable = true;
                enableRosetta = true;
                user = "kei";
              };
            }
          ];
      };
      darwinPackages = self.darwinConfigurations."Keis-MacBook-Pro".pkgs;
    };
}
