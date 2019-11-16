{ config, pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    rustup
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
