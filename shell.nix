{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-builder, bytestring, exceptions
      , ghc-prim, http-types, HUnit, mtl, parsec, QuickCheck, split
      , stdenv, test-framework, test-framework-hunit
      , test-framework-quickcheck2, test-framework-th, text, utf8-string
      }:
      mkDerivation {
        pname = "web-routes";
        version = "0.27.9";
        src = ./.;
        libraryHaskellDepends = [
          base blaze-builder bytestring exceptions ghc-prim http-types mtl
          parsec split text utf8-string
        ];
        testHaskellDepends = [
          base HUnit QuickCheck test-framework test-framework-hunit
          test-framework-quickcheck2 test-framework-th
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
