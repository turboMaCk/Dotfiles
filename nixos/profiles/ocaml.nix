{  pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    bs-platform
  ];
}
