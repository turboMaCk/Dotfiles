{ config, pkgs, ... }:
{
  networking.networkmanager = {
    enable = true;
    enableStrongSwan = true;
    extraConfig = ''
      [main]
      rc-manager=resolvconf
    '';
  };

  services.strongswan = {
    enable = true;
    secrets = [
      # see https://github.com/NixOS/nixpkgs/issues/64965
      "ipsec.d/ipsec.nm-l2tp.secrets"
    ];
  };
}
