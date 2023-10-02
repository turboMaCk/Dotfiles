{ config, pkgs, ... }:
{
  imports = [
    ./desktop-base.nix
  ];

  environment.systemPackages = with pkgs; [
    kate
    gwenview
    transmission-qt
    libsForQt5.kalk
  ];

  # X11 settings
  services.xserver = {
    desktopManager = {
      plasma5.enable = true;
    };
  };
}
