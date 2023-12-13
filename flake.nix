{
  description = "flakes for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, nur, home-manager, purescript-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        localSystem = { inherit system; };
        config = {
          allowUnfree = true;
        };
        overlays = [
          nur.overlay
          purescript-overlay.overlays.default
        ];
      };
      mkSystem = { hostname, username }: nixpkgs.lib.nixosSystem {
        inherit system pkgs;

        specialArgs = { inherit inputs username; };
        modules = [
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${username} = import ./machines/${hostname}/home-configuration.nix;
          }
          ./machines/${hostname}/configuration.nix
          ./modules
        ];
      };
    in
    {
      formatter.x86_64-linux = pkgs.nixpkgs-fmt;
      nixosConfigurations = {
        vega = mkSystem { hostname = "vega"; username = "helix"; };
        gic = mkSystem { hostname = "gic"; username = "gicw"; };
      };
    };
}
