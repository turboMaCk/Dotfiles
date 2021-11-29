{ config, pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    cargo
    cargo-generate
    cargo-web
    rustup

    # tools
    rust-analyzer
    rustfmt
    clippy

    # stdenv deps
    gcc
    gnumake
    openssl
    pkgconfig
    binutils
  ];
}
