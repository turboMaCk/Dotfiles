# Purpose of this global config is mostly
# ease of working with my xmonad config.
# For other Haskell projects it's better to have local nix definitions!

{ config, pkgs, ... }:
let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
{
  environment.systemPackages = with pkgs.haskellPackages; [
    pkgs.cabal-install
    pkgs.cabal2nix
    hindent
    stylish-haskell
    hasktags
    ghcid
    ghc
    zlib
    # first add caches before installing this!
    # (all-hies.unstableFallback.selection {
    #   selector = p: { inherit (p) ghc864 ghc863 ghc843; };
    # })

    # stack
  ];


  # Setup binary caches
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://nixcache.reflex-frp.org"
      "https://all-hies.cachix.org"
      "https://miso-haskell.cachix.org"
    ];
    binaryCachePublicKeys = [
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
      "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
    ];
    trustedUsers = [ "root" "marek" ];
  };
}
