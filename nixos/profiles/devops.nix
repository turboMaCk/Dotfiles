{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # gcloud + k8
    # google-cloud-sdk
    # kubectl
    # sops

    # NixOps
    nixops
  ];
}
