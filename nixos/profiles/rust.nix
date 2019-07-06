{ config, pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    rustup
    cargo
    gcc
    gnumake
    openssl
    pkgconfig
    binutils
  ];
}
