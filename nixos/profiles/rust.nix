{ config, pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    rustc
    cargo
    cargo-generate
    cargo-web
    # rustup # pulls incompatible binaries 2022-09-04

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
