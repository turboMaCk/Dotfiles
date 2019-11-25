{  pkgs, ... }:
let
  bs-platform-src =
    pkgs.fetchFromGitHub {
      owner = "turboMaCk";
      repo = "bs-platform.nix";
      rev = "c20e8dc8703ad7975c99d76b5779d31c86078d98";
      sha256 = "06wii6487crawi7ngbls59snvygqhh29jz5f9q106m3vp9jzy7h9";
    };

  bs-platform = with pkgs;
    import "${bs-platform-src}/bs-platform.nix" {
      inherit stdenv fetchFromGitHub nodejs ninja python35;
    };
in
{
  environment.systemPackages = [
    bs-platform
  ];
}
