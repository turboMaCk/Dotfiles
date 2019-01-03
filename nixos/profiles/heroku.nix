{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    heroku
  ];
}
