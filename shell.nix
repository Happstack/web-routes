with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, blaze-builder, bytestring, exceptions
             , ghc-prim, http-types, HUnit, mtl, parsec, QuickCheck, split
             , stdenv, test-framework, test-framework-hunit
             , test-framework-quickcheck2, test-framework-th, text, utf8-string
             }:
             mkDerivation {
               pname = "web-routes";
               version = "0.27.8";
               src = ./.;
               buildDepends = [
                 base blaze-builder bytestring exceptions ghc-prim http-types mtl
                 parsec split text utf8-string
               ];
               testDepends = [
                 base HUnit QuickCheck test-framework test-framework-hunit
                 test-framework-quickcheck2 test-framework-th
               ];
               description = "Library for maintaining correctness and composability of URLs within an application";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
