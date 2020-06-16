self: super:
let
  discord=
    { pname, version, src, binaryName, desktopName
    , autoPatchelfHook, fetchurl, makeDesktopItem, stdenv, wrapGAppsHook
    , alsaLib, at-spi2-atk, at-spi2-core, atk, cairo, cups, dbus, expat, fontconfig
    , freetype, gdk-pixbuf, glib, gtk3, libcxx, libdrm, libnotify, libpulseaudio, libuuid
    , libX11, libXScrnSaver, libXcomposite, libXcursor, libXdamage, libXext
    , libXfixes, libXi, libXrandr, libXrender, libXtst, libxcb
    , mesa, nspr, nss, pango, systemd
    }:

    let
      inherit binaryName;
    in stdenv.mkDerivation rec {
      inherit pname version src;

      nativeBuildInputs = [
        alsaLib
        autoPatchelfHook
        cups
        libdrm
        libuuid
        libX11
        libXScrnSaver
        libXtst
        libxcb
        mesa.drivers
        nss
        wrapGAppsHook
      ];

      dontWrapGApps = true;

      libPath = stdenv.lib.makeLibraryPath [
        libcxx systemd libpulseaudio
        stdenv.cc.cc alsaLib atk at-spi2-atk at-spi2-core cairo cups dbus expat fontconfig freetype
        gdk-pixbuf glib gtk3 libnotify libX11 libXcomposite libuuid
        libXcursor libXdamage libXext libXfixes libXi libXrandr libXrender
        libXtst nspr nss libxcb pango systemd libXScrnSaver
      ];

      installPhase = ''
    mkdir -p $out/{bin,opt/${binaryName},share/pixmaps}
    mv * $out/opt/${binaryName}

    chmod +x $out/opt/${binaryName}/${binaryName}
    patchelf --set-interpreter ${stdenv.cc.bintools.dynamicLinker} \
        $out/opt/${binaryName}/${binaryName}

    wrapProgram $out/opt/${binaryName}/${binaryName} \
        "''${gappsWrapperArgs[@]}" \
        --prefix XDG_DATA_DIRS : "${gtk3}/share/gsettings-schemas/${gtk3.name}/" \
        --prefix LD_LIBRARY_PATH : ${libPath}

    ln -s $out/opt/${binaryName}/${binaryName} $out/bin/
    ln -s $out/opt/${binaryName}/discord.png $out/share/pixmaps/${pname}.png

    ln -s "${desktopItem}/share/applications" $out/share/
  '';

      desktopItem = makeDesktopItem {
        name = pname;
        exec = binaryName;
        icon = pname;
        inherit desktopName;
        genericName = meta.description;
        categories = "Network;InstantMessaging;";
        mimeType = "x-scheme-handler/discord";
      };

      passthru.updateScript = ./update-discord.sh;

      meta = with stdenv.lib; {
        description = "All-in-one cross-platform voice and text chat for gamers";
        homepage = "https://discordapp.com/";
        downloadPage = "https://discordapp.com/download";
        license = licenses.unfree;
        maintainers = with maintainers; [ ldesgoui MP2E tadeokondrak ];
        platforms = [ "x86_64-linux" ];
      };
    };
in
{
  zasm = super.callPackage ../pkgs/zasm.nix {};

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

    elm-format = self.haskell.lib.dontCheck super.elmPackages.elm-format;
  };

  discord = self.callPackage discord rec {
    pname = "discord";
    binaryName = "Discord";
    desktopName = "Discord";
    version = "0.0.10";
    src = self.fetchurl {
      url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
      sha256 = "0kx92i8naqr3algmyy3wyzbh8146z7gigxwf1nbpg1gl16wlplaq";
    };
  };
}
