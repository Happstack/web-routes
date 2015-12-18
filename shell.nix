{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-builder, bytestring, exceptions
      , ghc-prim, hspec, http-types, HUnit, mtl, parsec, QuickCheck, split
      , stdenv, text, utf8-string
      }:
      mkDerivation {
        pname = "web-routes";
        version = "0.27.10";
        src = ./.;
        libraryHaskellDepends = [
          base blaze-builder bytestring exceptions ghc-prim http-types mtl
          parsec split text utf8-string
        ];
        testHaskellDepends = [
          base hspec HUnit QuickCheck
        ];
        description = "Library for maintaining correctness and composability of URLs within an application";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
