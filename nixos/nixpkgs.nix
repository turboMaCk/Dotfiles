{
  nixpkgs = {
    overlays = [
      (import ./overlays/pkgs.nix)
      (import ./overlays/haskell.nix)
      (import ./overlays/gnu-r.nix)
    ];
  };
}
