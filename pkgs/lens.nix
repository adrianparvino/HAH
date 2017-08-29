{ mkDerivation, array, base, base-orphans, bifunctors, bytestring
, comonad, containers, contravariant, criterion, deepseq
, distributive, exceptions, filepath, free, generic-deriving
, ghc-prim, hashable, hlint, HUnit, kan-extensions, mtl, parallel
, profunctors, QuickCheck, reflection, semigroupoids, semigroups
, stdenv, tagged, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, text, transformers, transformers-compat
, unordered-containers, vector, void
}:
mkDerivation {
  pname = "lens";
  version = "4.14";
  sha256 = "0jszxq3jk7yywy0dmkkdnl20fcmri2cl9b3cinw50a9mxwccv8vh";
  revision = "1";
  editedCabalFile = "1i2rh0a0d0y8jli09v7gnll7jx9f8fsicyqpr7329cw3ly7jabzc";
  libraryHaskellDepends = [
    array base base-orphans bifunctors bytestring comonad containers
    contravariant distributive exceptions filepath free ghc-prim
    hashable kan-extensions mtl parallel profunctors reflection
    semigroupoids semigroups tagged template-haskell text transformers
    transformers-compat unordered-containers vector void
  ];
  testHaskellDepends = [
    base containers hlint HUnit mtl QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 test-framework-th
    transformers
  ];
  benchmarkHaskellDepends = [
    base bytestring comonad containers criterion deepseq
    generic-deriving transformers unordered-containers vector
  ];
  doCheck = false;
  homepage = "http://github.com/ekmett/lens/";
  description = "Lenses, Folds and Traversals";
  license = stdenv.lib.licenses.bsd3;
}
