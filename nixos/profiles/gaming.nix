{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    (steam.override {
      extraPkgs = pkgs: [ openldap gdk-pixbuf ];
    }).run
  ];

  programs.steam.enable = true;
}
