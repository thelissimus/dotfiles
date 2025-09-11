{ pkgs, ... }:
let
  h = pkgs.haskell.packages."ghc912";
in
{
  home.file.".ghci".source = ../../.config/.ghci;

  home.packages = [
    h.ghc
    h.cabal-install
    h.haskell-language-server
    h.cabal-fmt
    h.fourmolu
    h.hlint
    h.ghcprofview
  ];
}
