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
}
