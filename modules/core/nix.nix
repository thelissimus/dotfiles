{ inputs, ... }: {
  nix.registry.nixpkgs.flake = inputs.nixpkgs;
  nix.settings = {
    sandbox = true;
    experimental-features = [ "nix-command" "flakes" ];
    trusted-public-keys = [
      "hackage-server.cachix.org-1:iw0iRh6+gsFIrxROFaAt5gKNgIHejKjIfyRdbpPYevY="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
    substituters = [
      "https://hackage-server.cachix.org/"
      "https://cache.iog.io"
      "https://cache.garnix.io"
    ];
  };
}
