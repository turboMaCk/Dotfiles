# See https://nixos.wiki/wiki/AMD_GPU
{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    vulkan-headers
    vulkan-loader
    vulkan-tools
  ];

  hardware.opengl.driSupport = true;
  hardware.opengl.extraPackages = with pkgs; [
    amdvlk
  ];
  # For 32 bit applications
  # Only available on unstable
  hardware.opengl.extraPackages32 = with pkgs; [
    driversi686Linux.amdvlk
  ];
}
