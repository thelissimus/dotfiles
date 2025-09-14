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
          ./modules/core/nix.nix
        ] ++ modules;
      };
    in
    {
      formatter.x86_64-linux = (mkPkgs "x86_64-linux").nixpkgs-fmt;
      formatter.aarch64-darwin = (mkPkgs "aarch64-darwin").nixpkgs-fmt;

      nixosConfigurations = {
        vega = let username = "helix"; hostname = "vega"; in mkSystem {
          system = "x86_64-linux";
          inherit username hostname;
          modules = [
            ./machines/${hostname}/conf.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${username} = import ./machines/vega/home.nix;
            }
            (import ./modules/services/postgres.nix {
              inherit (mkPkgs "x86_64-linux") pkgs;
              inherit username;
            })
          ];
        };
        fklr = let hostname = "fklr"; in mkSystem {
          system = "x86_64-linux";
          inherit hostname;
          username = "alice";
          modules = [
            ./machines/${hostname}/conf.nix
          ];
        };
      };

      darwinConfigurations."Keis-MacBook-Pro" = let pkgs = mkPkgs "aarch64-darwin"; in nix-darwin.lib.darwinSystem {
        inherit pkgs;
        modules =
          [
            ./modules/core/nix.nix
            (import ./machines/adlr/conf.nix { inherit pkgs; configurationRevision = self.rev or self.dirtyRev or null; })
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
