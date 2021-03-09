{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    renoise
  ];
}
