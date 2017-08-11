{ mkDerivation, base, miso, stdenv }:
mkDerivation {
  pname = "miso-persist";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base miso ];
  homepage = "https://github.com/cocreature/miso-persist#readme";
  license = stdenv.lib.licenses.bsd3;
}
