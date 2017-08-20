{ pkgs ? import <nixpkgs> {} }:
let
  ghcjs = pkgs.haskell.packages.ghcjsHEAD;
  miso-ghcjs = ghcjs.callPackage ./miso-ghcjs.nix {};
in
  {
    client = ghcjs.callPackage ./miso-middleware.nix { miso = miso-ghcjs; };
  }
