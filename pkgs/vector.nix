{ mkDerivation, base, deepseq, ghc-prim, primitive, QuickCheck
, random, stdenv, template-haskell, test-framework
, test-framework-quickcheck2, transformers
}:
mkDerivation {
  pname = "vector";
  version = "0.11.0.0";
  sha256 = "1r1jlksy7b0kb0fy00g64isk6nyd9wzzdq31gx5v1wn38knj0lqa";
  revision = "2";
  editedCabalFile = "1kjafhgsyjqlvrpfv2vj17hipyv0zw56a2kbl6khzn5li9szvyib";
  libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
  testHaskellDepends = [
    base QuickCheck random template-haskell test-framework
    test-framework-quickcheck2 transformers
  ];
  doCheck = false;
  homepage = "https://github.com/haskell/vector";
  description = "Efficient Arrays";
  license = stdenv.lib.licenses.bsd3;
}
