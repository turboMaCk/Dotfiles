{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    docker-compose
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
    virtualbox.host = {
      enable = false;
      enableExtensionPack = true;
      enableHardening = true;
    };
  };
}
