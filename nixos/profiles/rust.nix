{ config, pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    cargo
    cargo-generate

    # stdenv deps
    gcc
    gnumake
    openssl
    pkgconfig
    binutils
  ];
}
