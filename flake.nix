{
  description = "flakes for m[A]chines";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
  };

  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs {
        localSystem = { inherit system; };
        config = {
          allowUnfree = true;
        };
      };
    in
    {
      formatter.x86_64-linux = pkgs.nixpkgs-fmt;
      nixosConfigurations.vega = inputs.nixpkgs.lib.nixosSystem {
        inherit system;
        inherit pkgs;

        specialArgs = inputs;
        modules = [
          ./nixos/configuration.nix
          inputs.nur.nixosModules.nur
        ];
      };
    };
}
