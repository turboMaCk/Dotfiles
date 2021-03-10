{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    renoise
    qjackctl
    jack2
  ];
  # I want to use pulse audio as a primary
  # sounds server. jack is on demand
  # services.jack.jackd.enable = true;
}
