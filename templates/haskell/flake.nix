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

      perSystem = { system, pkgs, ... }:
        let
          hpkgs = pkgs.haskell.packages.ghc912;

          githooks = git-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              statix.enable = true;
              deadnix.enable = true;
              fourmolu.enable = true;
              cabal-fmt.enable = true;
              nixpkgs-fmt.enable = true;
            };
          };
        in
        {
          checks = {
            inherit githooks;
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
              ${githooks.shellHook}
            '';
          };
        };
    };
}
