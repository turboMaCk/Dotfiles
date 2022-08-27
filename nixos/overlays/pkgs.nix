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

  fourmolu = with self.haskell.lib; justStaticExecutables self.haskell.packages.ghc924.fourmolu;

  renoise = super.callPackage ../pkgs/renoise.nix {
    releasePath = /home/marek/.local/share/rns_331_linux_x86_64.tar.gz;
  };

  tall-reverb = super.callPackage ../pkgs/tal-reverb-4.nix {};
  tall-noisemaker = super.callPackage ../pkgs/tal-noisemaker.nix {};
  zebralette-mini-zebra = super.callPackage ../pkgs/mini-zebra.nix {};
  # carla = super.carla.overrideAttrs (old: {
  #   postFixup = old.postFixup + ''
  #     sed -i 's/--with-appname="$0"/--with-appname="carla"/g' $out/bin/.carla-wrapped
  #   '';
  # });
}
