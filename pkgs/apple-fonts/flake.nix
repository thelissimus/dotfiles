{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];

    perSystem = { pkgs, ... }: {
      packages =
        let
          makeAppleFont = name: pkgName: src: pkgs.stdenv.mkDerivation {
            inherit name src;

            version = "0.3.0";

            unpackPhase = ''
              undmg $src
              7z x '${pkgName}'
              7z x 'Payload~'
            '';

            buildInputs = [
              pkgs.undmg
              pkgs.p7zip
            ];

            setSourceRoot = "sourceRoot=`pwd`";

            installPhase = ''
              mkdir -p $out/share/fonts
              mkdir -p $out/share/fonts/opentype
              mkdir -p $out/share/fonts/truetype
              find -name \*.otf -exec mv {} $out/share/fonts/opentype/ \;
              find -name \*.ttf -exec mv {} $out/share/fonts/truetype/ \;
            '';
          };

          sources = {
            sf-pro = {
              url = "./SF-Pro.dmg";
              hash = "sha256-IccB0uWWfPCidHYX6sAusuEZX906dVYo8IaqeX7/O88=";
            };
          };
        in
        {
          sf-pro = makeAppleFont "sf-pro" "SF Pro Fonts.pkg" (pkgs.fetchurl sources.sf-pro);
        };
    };
  };
}
