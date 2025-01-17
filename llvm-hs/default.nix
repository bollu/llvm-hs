{ mkDerivation, array, base, bytestring, Cabal, containers, HUnit
, llvm-config, llvm-hs-pure, mtl, parsec, QuickCheck, stdenv
, template-haskell, temporary, test-framework, test-framework-hunit
, test-framework-quickcheck2, transformers, transformers-compat
, utf8-string
}:
mkDerivation {
  pname = "llvm-hs";
  version = "4.1.0.0";
  src = ./.;
  configureFlags = [ "-fshared-llvm" ];
  setupHaskellDepends = [ base Cabal containers ];
  libraryHaskellDepends = [
    array base bytestring containers llvm-hs-pure mtl parsec
    template-haskell transformers transformers-compat utf8-string
  ];
  libraryToolDepends = [ llvm-config ];
  testHaskellDepends = [
    base bytestring containers HUnit llvm-hs-pure mtl QuickCheck
    temporary test-framework test-framework-hunit
    test-framework-quickcheck2 transformers transformers-compat
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "General purpose LLVM bindings";
  license = stdenv.lib.licenses.bsd3;
}
