{ pkgs, username, ... }: {
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_15;
    ensureDatabases = [ username ];
    ensureUsers = [
      {
        name = username;
        ensureDBOwnership = true;
      }
    ];
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all              trust
      host  all all 127.0.0.1/32 trust
      host  all all ::1/128      trust
    '';
  };
}
