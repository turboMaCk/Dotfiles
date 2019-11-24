{  pkgs, ... }:
let
  bs-platform-src =
    pkgs.fetchFromGitHub {
      owner = "turboMaCk";
      repo = "bs-platform.nix";
      rev = "2483f8a2b8d47e48175c1eb84e1f0b61afe02fe3";
      sha256 = "1vjfgdyznz26awg2xkfw7s9a12bi58d2cbmmxkl8vywz0hkpsxy1";
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
