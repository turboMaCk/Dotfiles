# See https://nixos.wiki/wiki/AMD_GPU
{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    vulkan-headers
    vulkan-loader
    vulkan-tools
  ];
}
