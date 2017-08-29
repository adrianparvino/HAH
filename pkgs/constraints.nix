{ mkDerivation, base, binary, deepseq, ghc-prim, hashable, mtl
, stdenv, transformers, transformers-compat
}:
mkDerivation {
  pname = "constraints";
  version = "0.8";
  sha256 = "120mmv9rwbahslisc1z8zx9lw7v6hl5fzid4l0hiy5as6ijqgl2c";
  libraryHaskellDepends = [
    base binary deepseq ghc-prim hashable mtl transformers
    transformers-compat
  ];
  doCheck = false;
  homepage = "http://github.com/ekmett/constraints/";
  description = "Constraint manipulation";
  license = stdenv.lib.licenses.bsd3;
}
