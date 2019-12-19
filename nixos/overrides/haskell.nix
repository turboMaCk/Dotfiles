{ config, pkgs, ...}:
{
  nixpkgs.config = {
    packageOverrides = pkgs: {
      haskellPackages = super.haskellPackages.override {
        overrides = hsNew: hsOld: {
          snap-server = hsNew.overrideCabal hsOld.snap-server (drv: {
            patches = [
              (self.fetchpatch {
                url = https://github.com/snapframework/snap-server/pull/126/commits/4338fe15d68e11e3c7fd0f9862f818864adc1d45.patch;
                sha256 = "1nlw9lckm3flzkmhkzwc7zxhdh9ns33w8p8ds8nf574nqr5cr8bv";
              })
              (self.fetchpatch {
                 url = https://github.com/snapframework/snap-server/pull/126/commits/410de2df123b1d56b3093720e9c6a1ad79fe9de6.patch;
                 sha256 = "08psvw0xny64q4bw1nwg01pkzh01ak542lw6k1ps7cdcwaxk0n94";
              })
            ];
          });
        };
      };
    };
  };
}
