self: super:
rec {
  # See issues for mode details
  #   - https://github.com/input-output-hk/haskell.nix/issues/537#issuecomment-611322396
  #   - https://github.com/NixOS/nixpkgs/issues/67032#issuecomment-607732200
  liblapack = super.liblapack.override { shared = true; };

  fourmolu = with self.haskell.lib; justStaticExecutables self.haskellPackages.fourmolu;

  renoise = super.callPackage ../pkgs/renoise.nix {
    releasePath = /home/marek/.local/share/rns_344_linux_x86_64.tar.gz;
  };

  opentofu = self.callPackage ../pkgs/opentofu.nix {};

  tall-reverb = super.callPackage ../pkgs/tal-reverb-4.nix {};
  tall-noisemaker = super.callPackage ../pkgs/tal-noisemaker.nix {};
  zebralette-mini-zebra = super.callPackage ../pkgs/mini-zebra.nix {};
  # carla = super.carla.overrideAttrs (old: {
  #   postFixup = old.postFixup + ''
  #     sed -i 's/--with-appname="$0"/--with-appname="carla"/g' $out/bin/.carla-wrapped
  #   '';
  # });
}
