{
  description = "flakes for m[A]chines";

  inputs = {
    # nix modules
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-darwin.url = "github:nix-darwin/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # tools
    flake-parts.url = "github:hercules-ci/flake-parts";

    # overlays
    nur = {
      url = "github:nix-community/NUR";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
    k-framework.url = "github:runtimeverification/k";
    k-framework.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    # homebrew taps
    nikitabobko-tap = {
      url = "github:nikitabobko/homebrew-tap";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , nix-darwin
    , nix-homebrew
    , home-manager
    , flake-parts
    , ...
    }@inputs:
    let
      appleFontsOverlay = _: prev: {
        sf-pro = prev.stdenv.mkDerivation {
          name = "sf-pro";
          version = "0.3.0";

          src = ./.fonts/SF-Pro.dmg;

          unpackPhase = ''
            undmg $src
            7z x 'SF Pro Fonts.pkg'
            7z x 'Payload~'
          '';

          buildInputs = [
            prev.undmg
            prev.p7zip
          ];

          setSourceRoot = "sourceRoot=`pwd`";

          installPhase = ''
            mkdir -p $out/share/fonts
            mkdir -p $out/share/fonts/opentype
            mkdir -p $out/share/fonts/truetype
            find -name \*.otf -exec mv {} $out/share/fonts/opentype/ \;
            find -name \*.ttf -exec mv {} $out/share/fonts/truetype/ \;
          '';
        };
      };

      mkPkgs = system: import nixpkgs {
        localSystem = { inherit system; };
        config = {
          allowUnfree = true;
        };
        overlays = [
          appleFontsOverlay
          inputs.k-framework.overlay
          inputs.nur.overlays.default
          inputs.emacs-overlay.overlays.default
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
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.${username} = import home;
              };
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
          specialArgs = { inherit inputs system hostname username configurationRevision; };
          modules = [
            ./modules/core/nix.nix
            conf
            home-manager.darwinModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.${username} = import home;
              };
            }
            nix-homebrew.darwinModules.nix-homebrew
            {
              nix-homebrew = {
                enable = true;
                enableRosetta = true;
                user = username;
                taps = {
                  "nikitabobko/homebrew-tap" = inputs.nikitabobko-tap;
                };
                mutableTaps = false;
                autoMigrate = true;
              };
            }
            ({ config, ... }: {
              homebrew.taps = builtins.attrNames config.nix-homebrew.taps;
            })
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
