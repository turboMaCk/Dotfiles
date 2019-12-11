{ pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    ghostscript
    (pkgs.texlive.combine {
        inherit (texlive) scheme-basic blindtext geometry;
    })
  ];
}
