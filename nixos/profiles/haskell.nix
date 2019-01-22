# Purpose of this global config is mostly
# ease of working with my xmonad config.
# For other Haskell projects it's better to have local nix definitions!

{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs.haskellPackages; [
    pkgs.cabal-install
    pkgs.cabal2nix
    hindent
    stylish-haskell
    stack
    hasktags
    ghcid
    ghc
    zlib
    stack2nix
  ];

  # Enable GHC.js binary cache
  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
}
