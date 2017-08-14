{ mkDerivation, aeson, base, BoundedChan, bytestring, containers
, ghcjs-base, http-api-data, http-types, network-uri, scientific
, servant, stdenv, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "miso";
  version = "0.7.2.0";
  sha256 = "1xg2bvavgpz3b8sgp4lbxszznvcbhhrzsy31lwalv5w1vcfjkwsw";
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
