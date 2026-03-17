{ pkgs, ... }:
let
  h = pkgs.haskell.packages.ghc912;
  hpkgs = pkgs.haskellPackages;
  htools = pkgs.buildEnv {
    name = "haskell-tooling";
    paths = [
      h.cabal-install
      h.haskell-language-server
      hpkgs.cabal-fmt
      h.fourmolu
      h.hlint
      h.hoogle
      h.cabal-hoogle
    ];
    pathsToLink = [
      "/bin"
      "/share"
    ];
  };
in
{
  home.file.".ghci".source = ../../.config/.ghci;

  home.packages = [
    h.ghc
    htools
    # hpkgs.ghcprofview
  ];
}
