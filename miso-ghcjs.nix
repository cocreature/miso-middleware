{ mkDerivation, aeson, base, BoundedChan, bytestring, containers
, ghcjs-base, http-api-data, http-types, network-uri, scientific
, servant, stdenv, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "miso";
  version = "0.7.1.0";
  sha256 = "05g10p3z7rb5wp19hzjf3hhgxbkf5j9h4wh7q5fa3d0ls4bhd71d";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base BoundedChan bytestring containers ghcjs-base
    http-api-data http-types network-uri scientific servant text
    transformers unordered-containers vector
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
