{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    steam
    # (steam.override {
    #   extraPkgs = pkgs: [ openldap ];
    #   nativeOnly = true;
    # }).run
  ];

  # Steam requirements
  # https://nixos.org/nixpkgs/manual/#sec-steam
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
}
