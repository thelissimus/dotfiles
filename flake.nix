{
  description = "flakes for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, nur, home-manager }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        localSystem = { inherit system; };
        config = {
          allowUnfree = true;
        };
        overlays = [
          nur.overlay
        ];
      };
    in
    {
      formatter.x86_64-linux = pkgs.nixpkgs-fmt;
      nixosConfigurations.vega = nixpkgs.lib.nixosSystem {
        inherit system pkgs;

        specialArgs = { inherit inputs; };
        modules = [
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.helix = import ./nixos/home-configuration.nix;
          }
          ./nixos/configuration.nix
        ];
      };
    };
}
