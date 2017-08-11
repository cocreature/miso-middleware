{ pkgs ? import <nixpkgs> {} }:
let
  miso-ghcjs = pkgs.haskell.packages.ghcjs.callPackage ./miso-ghcjs.nix {};
in
  {
    client = pkgs.haskell.packages.ghcjs.callPackage ./miso-middleware.nix { miso = miso-ghcjs; };
  }
