{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    steam
    (steam.override {
      extraPkgs = pkgs: [ openldap gdk-pixbuf ];
    }).run
  ];
}
