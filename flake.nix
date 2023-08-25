{
  description = "flowers for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
  };

  outputs = inputs @ { self, nixpkgs, home-manager, nur, ... }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      formatter.x86_64-linux = pkgs.nixpkgs-fmt;
      nixosConfigurations.vega = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = inputs;
        modules = [
          ./nixos/configuration.nix
          nur.nixosModules.nur
        ];
      };
    };
}
