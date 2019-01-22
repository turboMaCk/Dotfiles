{ config, pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    rustc
    cargo
    gcc
    gnumake
    openssl
    pkgconfig
  ];
}
