adlr:
  sudo darwin-rebuild switch --flake .#Keis-MacBook-Pro

eval-adlr:
  nix eval .#darwinConfigurations.Keis-MacBook-Pro.config.system.build.toplevel.drvPath --show-trace

eval-vega:
  nix eval .#nixosConfigurations.vega.config.system.build.toplevel.drvPath --show-trace

eval-fklr:
  nix eval .#nixosConfigurations.fklr.config.system.build.toplevel.drvPath --show-trace

eval: eval-adlr eval-vega eval-fklr
