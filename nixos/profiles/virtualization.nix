{ config, pkgs, ... }:

{
  # Virtualization & Docker

  environment.systemPackages = with pkgs; [
    docker_compose
  ];

  virtualisation = {
    docker = {
      enable = true;
      autoPrune = {
        enable = true;
        flags = [ "--all"  "--volumes" ];
      };
    };

    virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
      enableHardening = true;
    };
  };
}
