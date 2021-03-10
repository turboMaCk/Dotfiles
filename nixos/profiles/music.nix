{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    renoise
    qjackctl
  ];
  # services.jack.jackd.enable = true;
}
