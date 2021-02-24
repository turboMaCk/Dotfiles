{ config, pkgs, ... }:
{
  imports = [
    ./desktop-base.nix
  ];

  environment.systemPackages = with pkgs; [];

  # X11 settings
  services.xserver = {
    desktopManager = {
      plasma5.enable = true;
    };

    displayManager = {
      defaultSession = "plasma5";
    };
  };
}
