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

    # set on the machine level for now
    # virtualbox.host = {
    #   enable = true;
    #   # enableExtensionPack = true;
    #   # enableHardening = true;
    # };
  };
}
