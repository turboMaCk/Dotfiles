{ stdenv, unzip, alsaLib, llvmPackages, freetype }:

stdenv.mkDerivation {
  pname = "TAL-NoiseMaker";
  version = "4.6.2";
  src = builtins.fetchurl "https://tal-software.com/downloads/plugins/TAL-NoiseMaker_64_linux.zip";
  nativeBuildInputs = [ unzip ];
  sourceRoot = ".";
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/lib/vst
    cp libTAL-NoiseMaker.so $out/lib/vst
  '';

  postFixup = ''
    patchelf \
      --set-rpath ${alsaLib}/lib:${stdenv.cc.cc.lib}/lib:${freetype}/lib \
      $out/lib/vst/libTAL-NoiseMaker.so

    chmod +x $out/lib/vst/libTAL-NoiseMaker.so
  '';
}
