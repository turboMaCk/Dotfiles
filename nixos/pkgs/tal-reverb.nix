{ stdenv, unzip, alsaLib, llvmPackages, freetype }:

stdenv.mkDerivation {
  pname = "TAL-Reverb";
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
      --set-rpath ${alsaLib}/lib:$out/lib/vst/libTAL-Reverb-4.so \
      --set-rpath ${stdenv.cc.cc.lib}/lib:$out/lib/vst/libTAL-Reverb-4.so \
      --set-rpath ${freetype}/lib:$out/lib/vst/libTAL-Reverb-4.so \
      $out/lib/vst/libTAL-Reverb-4.so

    chmod +x $out/lib/vst/libTAL-Reverb-4.so
  '';
}
