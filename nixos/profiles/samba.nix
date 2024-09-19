{ config, pkgs, ... }:
{
  services.samba = {
    enable = true;
    nsswins = true;
    winbindd.enable = true;
  };
  services.samba-wsdd = {
    enable = true;
    discovery = true;
  };
  networking.firewall.allowedTCPPorts = [ 139 445 5357 ];
  networking.firewall.allowedUDPPorts = [ 137 138 3702 ];

  environment.systemPackages = [
    pkgs.cifs-utils
  ];
}
