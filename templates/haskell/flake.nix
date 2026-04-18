{
  description = "";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { flake-parts
    , git-hooks
    , ...
    }@inputs: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem = { system, config, pkgs, ... }:
        let
          hpkgs = pkgs.haskell.packages.ghc912;

          template = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "" ./. { }) (_: {
            doCheck = true;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
          });
        in
        {
          imports = [
            git-hooks.flakeModule
          ];

          packages.default = template;

          pre-commit.settings.hooks = {
            statix.enable = true;
            deadnix.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
            nixpkgs-fmt.enable = true;
          };

          devShells.default = pkgs.mkShell {
            packages = [
              hpkgs.cabal-install
              hpkgs.haskell-language-server
              hpkgs.fourmolu
              hpkgs.hlint
              hpkgs.implicit-hie
              hpkgs.ghc
              pkgs.haskellPackages.cabal-fmt
              hpkgs.hoogle
              hpkgs.cabal-hoogle
            ];

            shellHook = ''
              ${config.pre-commit.installationScript}
            '';
          };
        };
    };
}
