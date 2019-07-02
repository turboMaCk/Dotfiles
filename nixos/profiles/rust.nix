{ config, pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    rustc
    cargo
    cargo
    gcc
    gnumake
    openssl
    pkgconfig
    binutils
  ];
}
