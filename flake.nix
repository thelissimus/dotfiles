{
  description = "flakes for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, nur, home-manager, ... }:
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
            home-manager.users.helix = import ./machines/vega/home-configuration.nix;
          }
          ./machines/vega/configuration.nix
        ];
      };
      nixosConfigurations.gic = nixpkgs.lib.nixosSystem {
        inherit system pkgs;

        specialArgs = { inherit inputs; };
        modules = [
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.gicw = import ./machines/gic/home-configuration.nix;
          }
          ./machines/gic/configuration.nix
        ];
      };
    };
}
