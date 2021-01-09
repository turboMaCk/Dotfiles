{ config, pkgs, ... }:
{
  hardware.pulseaudio = {
    # Sound config
    enable = true;
    package = pkgs.pulseaudioFull;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    support32Bit = true;
  };
}
