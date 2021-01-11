# Purpose of this global config is mostly
# ease of working with my xmonad config.
# For other Haskell projects it's better to have local nix definitions!

{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs.haskellPackages; [
    pkgs.cabal-install
    pkgs.cabal2nix
    hlint
    # hindent
    hasktags
    # stylish-haskell using stack installation because 0.12.0.0
    hasktags
    ghcid
    ghc
    zlib
    stack
  ];


  # Setup binary caches
  nix = {
    binaryCaches = [
      "https://hydra.iohk.io"
      "https://cache.nixos.org/"
      "https://nixcache.reflex-frp.org"
      # "https://all-hies.cachix.org"
      "https://miso-haskell.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      # "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
      "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
    ];
    trustedUsers = [ "root" "marek" ];
  };
}
