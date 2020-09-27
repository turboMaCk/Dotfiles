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
    stylish-haskell
    hasktags
    ghcid
    ghc
    zlib
    pks.haskell.packages.ghc865
    stack
  ];


  # Setup binary caches
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://nixcache.reflex-frp.org"
      # "https://all-hies.cachix.org"
      "https://miso-haskell.cachix.org"
    ];
    binaryCachePublicKeys = [
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      # "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
      "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
    ];
    trustedUsers = [ "root" "marek" ];
  };
}
