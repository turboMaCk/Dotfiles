self: super:
{
  haskellPackages = super.haskellPackages.override {
    overrides = nself: nsuper: {
      # patch hakyll to include watchServer and previewServer
      hakyll = nsuper.hakyll.overrideAttrs(old: {
        configureFlags = "-f watchServer -f previewServer";
        patches = [ ./hakyll.patch ];
      });
    };
  };
}
