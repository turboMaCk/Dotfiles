{ config, pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    cargo
    cargo-generate
    cargo-web

    # tools
    rust-analyzer
    clippy

    # stdenv deps
    gcc
    gnumake
    openssl
    pkgconfig
    binutils
  ];
}
