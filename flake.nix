{
  description = "flakes for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nur.url = "github:nix-community/NUR";
    k.url = "github:runtimeverification/k";
    k.inputs.nixpkgs.follows = "nixpkgs";
    apple-fonts.url = "path:pkgs/apple-fonts";
    apple-fonts.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , nix-darwin
    , nix-homebrew
    , home-manager
    , flake-parts
    , apple-fonts
    , nur
    , k
    , ...
    }@inputs:
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
          (_: _: {
            apple-fonts = apple-fonts.packages.${system};
          })
          k.overlay
          nur.overlays.default
        ];
      };

      mkNixos =
        { system
        , hostname
        , username
        , conf
        , home ? null
        , modules ? [ ]
        }:
        let pkgs = mkPkgs system; in
        nixpkgs.lib.nixosSystem {
          inherit system pkgs;
          specialArgs = { inherit inputs hostname username; };
          modules = [
            ./modules/core/nix.nix
            conf
          ]
          ++ (if home == null then [ ] else [
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${username} = import home;
            }
          ])
          ++ modules;
        };

      mkDarwin =
        { system
        , hostname
        , username
        , conf
        , home
        , modules ? [ ]
        , configurationRevision
        }:
        let pkgs = mkPkgs system; in nix-darwin.lib.darwinSystem {
          inherit pkgs;
          specialArgs = { inherit inputs hostname username configurationRevision; };
          modules = [
            ./modules/core/nix.nix
            conf
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${username} = import home;
            }
            nix-homebrew.darwinModules.nix-homebrew
            {
              nix-homebrew = {
                enable = true;
                enableRosetta = true;
                user = username;
              };
            }
          ] ++ modules;
        };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem = { system, pkgs, ... }: {
        _module.args.pkgs = mkPkgs system;
        formatter = pkgs.nixpkgs-fmt;
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            nixpkgs-fmt
            deadnix
            statix
          ];
        };
      };

      flake = {
        nixosConfigurations = {
          vega = mkNixos {
            system = "x86_64-linux";
            hostname = "vega";
            username = "helix";
            conf = ./machines/vega/conf.nix;
            home = ./machines/vega/home.nix;
            modules = [
              ./modules/services/postgres.nix
            ];
          };

          fklr = mkNixos {
            system = "x86_64-linux";
            hostname = "fklr";
            username = "alice";
            conf = ./machines/fklr/conf.nix;
          };
        };

        darwinConfigurations = {
          "Keis-MacBook-Pro" = mkDarwin {
            system = "aarch64-darwin";
            hostname = "adlr";
            username = "kei";
            conf = ./machines/adlr/conf.nix;
            home = ./machines/adlr/home.nix;
            configurationRevision = self.rev or self.dirtyRev or null;
          };
        };
        darwinPackages = self.darwinConfigurations."Keis-MacBook-Pro".pkgs;
      };
    };
}
