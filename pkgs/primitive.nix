{ mkDerivation, base, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "primitive";
  version = "0.6.1.0";
  sha256 = "1j1q7l21rdm8kfs93vibr3xwkkhqis181w2k6klfhx5g5skiywwk";
  revision = "1";
  editedCabalFile = "0gb8lcn6bd6ilfln7ah9jmqq6324vgkrgdsnz1qvlyj3bi2w5ivf";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  testHaskellDepends = [ base ghc-prim ];
  doCheck = false;
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = stdenv.lib.licenses.bsd3;
}
