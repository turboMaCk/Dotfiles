{ stdenv, lib, fetchgit, nodejs-8_x,
base, Cabal, directory, filepath, process }:

let
  rpath = lib.makeLibraryPath [
     nodejs-8_x
  ];

in stdenv.mkDerivation {
  pname = "elm-verify-examples";
  version = "2.3.1";
  src = fetchgit {
      url = "https://github.com/stoeffel/elm-verify-examples";
      sha256 = "1w5vqbazjkbv8dp728gl143wa885imzsph8pckgxykpinjbqdrpm";
      rev = "001f1a646ef35eaf0d968d40c04fe91b2b4fd6f1";
  };
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory filepath process ];

  buildPhase = ''
    source $stdenv/setup
    npm run build
  '';
  license = stdenv.lib.licenses.bsd3;
}
