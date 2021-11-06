{ config, pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    cargo
    cargo-generate
    cargo-web

    # stdenv deps
    gcc
    gnumake
    openssl
    pkgconfig
    binutils
  ];
}
