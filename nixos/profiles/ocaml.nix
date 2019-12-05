{  pkgs, ... }:
let
  bs-platform-src =
    pkgs.fetchFromGitHub {
      owner = "turboMaCk";
      repo = "bs-platform.nix";
      rev = "e37dbb37be393739c9ee0a0dc9c315229ad4a9ed";
      sha256 = "1kjj1mqw46l1y2p2z7hc2s80v1fcqr13jgb62l6xgv88dvr9499y";
    };

  bs-platform = with pkgs;
    import "${bs-platform-src}/bs-platform.nix" {
      inherit stdenv fetchFromGitHub nodejs ninja python3;
    };
in
{
  environment.systemPackages = [
    bs-platform
  ];
}
