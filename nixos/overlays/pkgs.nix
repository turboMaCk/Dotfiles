self: super:
{
  # See issues for mode details
  #   - https://github.com/input-output-hk/haskell.nix/issues/537#issuecomment-611322396
  #   - https://github.com/NixOS/nixpkgs/issues/67032#issuecomment-607732200
  liblapack = super.liblapack.override { shared = true; };

  emacs26 = with super;
    callPackage ./emacs26.nix {
      # use override to enable additional features
      libXaw = xorg.libXaw;
      Xaw3d = null;
      gconf = null;
      alsaLib = null;
      imagemagick = null;
      acl = null;
      gpm = null;
      inherit (darwin.apple_sdk.frameworks) AppKit GSS ImageIO;
    };

  fourmolu = with self.haskell.lib; justStaticExecutables self.haskellPackages.fourmolu;

  # magnetophonDSP = super.magnetophonDSP // {
  #   MBdistortion = super.magnetophonDSP.MBdistortion.override {
  #     version = "1.1.1";
  #     src = self.fetchurl {
  #       url = "https://github.com/turboMaCk/MBdistortion/archive/10e35084b88c559f1b63760cf40fd5ef5a6745a5.tar.gz";
  #       sha256 = "0z3p20r2mrcg58bff1b56gf6lsdgxx49g3i8r430d8jyqhim5r63";
  #     };
  #   };
  # };

  renoise = super.callPackage ../pkgs/renoise.nix {
    releasePath = /home/marek/.local/share/rns_331_linux_x86_64.tar.gz;
  };

  tall-reverb = super.callPackage ../pkgs/tal-reverb-4.nix {};
  tall-noisemaker = super.callPackage ../pkgs/tal-noisemaker.nix {};
  zebralette-mini-zebra = super.callPackage ../pkgs/mini-zebra.nix {};
}
