{ config, pkgs, ... }:

{
  # Virtualization & Docker

  environment.systemPackages = with pkgs; [
    docker_compose
  ];

  virtualisation = {
    docker.enable = true;

    virtualbox = {
        host.enable = true;
    };
  };
}
