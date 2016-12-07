{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
let
  callPackage = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage;
in
callPackage ./UMM.nix {}
