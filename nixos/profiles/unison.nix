{ config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    unison-ucm
  ];
}
