{ config, pkgs, ... }:
{
  imports = [
    ./desktop-base.nix
  ];

  environment.systemPackages = with pkgs; [
    kate
    gwenview
    transmission_3-qt
    libsForQt5.kalk
    qdirstat
  ];

  # X11 settings
  services.xserver = {
    desktopManager = {
      plasma5.enable = true;
    };
  };
}
