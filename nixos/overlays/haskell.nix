self: super:
{
  haskellPackages = super.haskellPackages.override {
    overrides = nself: nsuper: {
      # it depends on HsYAML >=0.2.0 && < 0.3, so use 0.2.1.0
      HsYAML-aeson = nsuper.HsYAML-aeson.override {
        HsYAML = nself.HsYAML_0_2_1_0;
      };

      # it depends on HsYAML >=0.2.0 && < 0.3, so use 0.2.1.0
      stylish-haskell = nsuper.stylish-haskell.override {
        HsYAML = nself.HsYAML_0_2_1_0;
      };
    };
  };
}
