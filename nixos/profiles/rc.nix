{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    opentx
    betaflight-configurator
  ];
}
