self: super:
{
  zasm = super.callPackage ../pkgs/zasm.nix {};

  discord = super.discord.overrideAttrs (old: rec {
    version = "0.0.10";
    src = super.fetchurl {
      url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
      sha256 = "0kx92i8naqr3algmyy3wyzbh8146z7gigxwf1nbpg1gl16wlplaq";
    };
  });

  # See issues for mode details
  #   - https://github.com/input-output-hk/haskell.nix/issues/537#issuecomment-611322396
  #   - https://github.com/NixOS/nixpkgs/issues/67032#issuecomment-607732200
  liblapack = super.liblapack.override { shared = true; };

  elmPackages = super.elmPackages // {
    create-elm-app = super.elmPackages.create-elm-app.override (old: {
      postInstall = old.postInstall + ''
        ln -sf ${self.elmPackages.elm}/bin/elm $out/lib/node_modules/create-elm-app/node_modules/elm/bin
      '';
    });
  };
}
