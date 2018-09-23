# Purpose of this global config is mostly
# ease of working with my xmonad config.
# For other Haskell projects it's better to have local nix definitions!

{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs.haskellPackages; [
    ghc
    # cabal # cabal is marked as broken
    cabal2nix
    stack
  ];
}
