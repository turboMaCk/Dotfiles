{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    pavucontrol
    jack_capture
    qjackctl
    jack2Full
  ];

  # services.jack = {
  #   jackd = {
  #     enable = true;
  #     extraOptions = [ "-dalsa" "--device" "hw:1,0" ];
  #     package = pkgs.jack2Full;
  #   };
  # };

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    support32Bit = true;
  };

  # NOTE: not needed with musnix which takes care of this implicitely
  security.pam.loginLimits = [
    { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
    { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
    { domain = "@audio"; item = "nofile"; type = "soft"; value = "99999"; }
    { domain = "@audio"; item = "nofile"; type = "hard"; value = "99999"; }
  ];
}
