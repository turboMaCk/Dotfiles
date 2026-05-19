{ config, pkgs, ... }:
{
  imports = [
    ./desktop-base.nix
  ];

  environment.systemPackages = with pkgs; [
    kdePackages.kate
    kdePackages.gwenview
    transmission_4-qt
    kdePackages.kalk
    qdirstat
    wl-clipboard
    kdePackages.kdeplasma-addons
  ];

  # X11 settings
  services.desktopManager = {
    plasma6.enable = true;
  };

  programs.kdeconnect.enable = true;

  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
  };
}
