{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    networkmanager-l2tp
    networkmanager_strongswan
  ];

  networking.networkmanager = {
    enable = true;
    plugins = with pkgs; [
      networkmanager-openvpn
    ];
    settings = {
      main.rc-manager = "resolvconf";
    };
  };

  services.strongswan = {
    enable = true;
    secrets = [
      # see https://github.com/NixOS/nixpkgs/issues/64965
      "ipsec.d/ipsec.nm-l2tp.secrets"
    ];
  };
}
