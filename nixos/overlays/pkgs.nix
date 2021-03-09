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

  fourmolu = with self.haskell.lib; justStaticExecutables
    (overrideCabal self.haskellPackages.fourmolu (old: {
      version = "0.3.0.0";
      src = fetchTarball https://github.com/parsonsmatt/fourmolu/archive/45a8478b8e6ba48b4ce228d4aaee3cb9f5aa08f6.tar.gz;
    }));

  discord = super.discord.override rec {
    version = "0.0.13";
    src = self.fetchurl {
      url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
      sha256 = "0d5z6cbj9dg3hjw84pyg75f8dwdvi2mqxb9ic8dfqzk064ssiv7y";
    };
  };

  renoise = super.callPackage ../pkgs/renoise.nix {};
}
