{ inputs, ... }: {
  nix.registry.nixpkgs.flake = inputs.nixpkgs;
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
  };
}
