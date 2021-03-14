{ stdenv, libuuid, libxcb, expat, gtk3, glib }:

stdenv.mkDerivation {
  pname = "Zebralette-mini-Zebra";
  version = "2.9.2";
  src = builtins.fetchurl "https://dl.u-he.com/releases/Zebra2_292_10721_Linux.tar.xz";
  dontBuild = true;
  sourceRoot = "Zebra2-10721";

  installPhase = ''
    mkdir -p $out/lib/vst
    cp -r Zebra2/* $out/lib

    ln -f $out/lib/Zebra2.64.so $out/lib/vst/Zebra2.64.so
  '';

  postFixup = ''
    patchelf \
      --set-rpath ${expat}/lib:${libxcb}/lib:${libuuid.out}/lib \
      $out/lib/vst/Zebra2.64.so

    patchelf \
      --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
      --set-rpath ${gtk3}/lib:${glib.out}/lib \
      $out/lib/dialog.64
  '';
}
