{ mkDerivation, base, blaze-builder, bytestring, exceptions
, ghc-prim, hspec, http-types, HUnit, mtl, parsec, QuickCheck
, split, stdenv, text, utf8-string
}:
mkDerivation {
  pname = "web-routes";
  version = "0.27.10";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-builder bytestring exceptions ghc-prim http-types mtl
    parsec split text utf8-string
  ];
  testHaskellDepends = [ base hspec HUnit QuickCheck ];
  homepage = "http://www.happstack.com/docs/crashcourse/index.html#web-routes";
  description = "portable, type-safe URL routing";
  license = stdenv.lib.licenses.bsd3;
}
