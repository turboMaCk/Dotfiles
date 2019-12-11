{ pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    ghostscript
    (pkgs.texlive.combine {
      inherit (texlive)
        scheme-basic
        collection-fontsextra
        collection-fontsrecommended
        collection-fontutils
        collection-basic

        blindtext
        geometry
        microtype
        graphics
        wrapfig
        enumitem
        fancyhdr
        amsmath
        index
        titlesec
        titling
        opensans
        xcolor
        fontspec
        frame
        framed
      ;
    })
  ];
}
