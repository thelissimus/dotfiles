{ ... }: {
  programs.zed-editor = {
    enable = true;
    extensions = [
      "haskell"
      "nix"
      "toml"
      "dockerfile"
      "git-firefly"
      "svelte"
    ];
    userSettings = builtins.fromJSON (builtins.readFile ../../.config/zed/settings.json);
  };
}
