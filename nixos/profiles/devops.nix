{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # gcloud + k8
    google-cloud-sdk
    kubectl
    sops

    # NixOps
    # requirtes patch https://github.com/NixOS/nixpkgs/pull/91047
    # nixops
  ];
}
