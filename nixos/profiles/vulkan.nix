# See https://nixos.wiki/wiki/AMD_GPU
{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    vulkan-headers
    vulkan-loader
    vulkan-tools
  ];

  hardware.opengl = {
    enable = true;
    # Vulkan
    driSupport = true;
    driSupport32Bit = true;

    # Disabling amdvlk
    # see https://github.com/bevyengine/bevy/issues/3288#issuecomment-1004056533
  };
}
