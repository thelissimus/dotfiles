{
  description = "flakes for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    apple-fonts.url = "path:pkgs/apple-fonts";
    apple-fonts.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, nur, home-manager, apple-fonts, ... }:
    let
      system = "x86_64-linux";
      overlay = final: prev: {
        apple-fonts = apple-fonts.packages.${system};
      };
      pkgs = import nixpkgs {
        localSystem = { inherit system; };
        config = {
          allowUnfree = true;
        };
        overlays = [
          overlay
          nur.overlay
        ];
      };
      mkSystem = { hostname, username, modules ? [] }: nixpkgs.lib.nixosSystem {
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
          ./modules/core/nix.nix
        ] ++ modules;
      };
    in
    {
      formatter.x86_64-linux = pkgs.nixpkgs-fmt;
      nixosConfigurations = {
        vega = mkSystem { hostname = "vega"; username = "helix"; };
      };
    };
}
