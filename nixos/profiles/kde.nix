{ config, pkgs, ... }:
{
  imports = [
    ./desktop-base.nix
  ];

  environment.systemPackages = with pkgs; [
    kdePackages.kate
    kdePackages.gwenview
    transmission_3-qt
    libsForQt5.kalk
    qdirstat
    wl-clipboard
  ];

  # X11 settings
  services.desktopManager = {
    plasma6.enable = true;
  };

  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
  };
}
