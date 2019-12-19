{ pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    ghostscript
    (texlive.combine {
      inherit (texlive)
        # Sick of installing plugins on by one
        collection-basic
        collection-latexextra
        collection-bibtexextra
        collection-latexrecommended
        collection-binextra
        collection-langeuropean
        collection-luatex
        collection-context
        collection-mathscience
        collection-fontsextra
        collection-langgerman
        collection-metapost
        collection-fontsrecommended
        collection-music
        collection-fontutils
        collection-pictures
        collection-formatsextra
        collection-plaingeneric
        collection-langkorean
        collection-pstricks
        collection-humanities
        collection-langother
        collection-publishers
        collection-texworks
        collection-wintools
        collection-xetex
        collection-latex

        scheme-small
        blindtext
        geometry
        microtype
        graphics
        wrapfig
        enumitem
        fancyhdr
        amsmath
        index
        fontawesome5
        titling
        xcolor
        titlesec
        multicolrule
        shapes
        fontspec
        bchart
      ;
    })
  ];
}
