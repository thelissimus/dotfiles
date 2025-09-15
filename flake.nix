{
  description = "flakes for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    k.url = "github:runtimeverification/k";
    k.inputs.nixpkgs.follows = "nixpkgs";
    apple-fonts.url = "path:pkgs/apple-fonts";
    apple-fonts.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs @ { self
    , nixpkgs
    , nix-darwin
    , nix-homebrew
    , home-manager
    , devshell
    , apple-fonts
    , nur
    , k
    , ...
    }:
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
          devshell.overlays.default
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
        }:
        let pkgs = mkPkgs system; in nix-darwin.lib.darwinSystem {
          inherit pkgs;
          specialArgs = { inherit inputs hostname username; configurationRevision = self.rev or self.dirtyRev or null; };
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

      systems = [ "x86_64-linux" "aarch64-darwin" ];
      mkDevShell = system:
        let pkgs = mkPkgs system; in pkgs.devshell.mkShell {
          packages = with pkgs; [
            nixpkgs-fmt
            deadnix
            statix
          ];
          commands = [
            {
              name = "fmt";
              command = "nix fmt .";
            }
            {
              name = "check";
              command = "nix flake check";
            }
            {
              name = "deadnix-check";
              command = "deadnix .";
            }
            {
              name = "statix-check";
              command = "statix check .";
            }
          ];
        };
    in
    {
      legacyPackages = nixpkgs.lib.genAttrs systems (system: mkPkgs system);
      formatter = nixpkgs.lib.genAttrs systems (system: (mkPkgs system).nixpkgs-fmt);
      devShells = nixpkgs.lib.genAttrs systems (system: { default = mkDevShell system; });

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
        };
      };
      darwinPackages = self.darwinConfigurations."Keis-MacBook-Pro".pkgs;
    };
}
