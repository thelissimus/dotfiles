{
  description = "flakes for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    apple-fonts.url = "path:./pkgs/apple-fonts";
    apple-fonts.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, nix-darwin, home-manager, apple-fonts, nur, ... }:
    let
      mkPkgs = system: import nixpkgs {
        localSystem = { inherit system; };
        config = {
          allowUnfree = true;
        };
        overlays = [
          (final: prev: {
            apple-fonts = apple-fonts.packages.${system};
          })
          nur.overlay
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

              services.nix-daemon.enable = true;
              nix.settings.experimental-features = "nix-command flakes";
              programs.zsh.enable = true;
              system.configurationRevision = self.rev or self.dirtyRev or null;
              system.stateVersion = 5;
              nixpkgs.hostPlatform = "aarch64-darwin";
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
          ];
      };
      darwinPackages = self.darwinConfigurations."Keis-MacBook-Pro".pkgs;
    };
}
