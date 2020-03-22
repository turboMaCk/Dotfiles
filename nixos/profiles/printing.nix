{ config, pkgs, ... }:

{
  # Enable CUPS to print documents.
  # helpful resources:
  #   - https://mypersonalblog1984.wordpress.com/2016/01/09/cups-unable-to-automatically-locate-printer-2/
  #   - https://github.com/NixOS/nixpkgs/issues/5409
  services.printing = {
    enable = true;
    drivers = with pkgs; [ hplipWithPlugin ];
  };
  # enable service discovery (useful for network printers)
  services.avahi = {
    enable = true;
    nssmdns = true;
  };
}
