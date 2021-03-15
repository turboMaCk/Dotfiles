{ stdenv, unzip, alsaLib, llvmPackages, freetype }:

stdenv.mkDerivation {
  pname = "TAL-Reverb-4";
  version = "4.64";
  src = builtins.fetchurl "https://tal-software.com/downloads/plugins/TAL-Reverb-4_64_linux.zip";
  nativeBuildInputs = [ unzip ];
  sourceRoot = ".";
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/lib/vst
    cp libTAL-Reverb-4.so $out/lib/vst
  '';

  postFixup = ''
    patchelf \
      --set-rpath ${alsaLib}/lib:${stdenv.cc.cc.lib}/lib:${freetype}/lib \
      $out/lib/vst/libTAL-Reverb-4.so

    chmod +x $out/lib/vst/libTAL-Reverb-4.so
  '';
}
