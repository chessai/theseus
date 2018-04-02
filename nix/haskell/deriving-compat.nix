{ mkDerivation, base, base-compat, base-orphans, containers
, fetchgit, ghc-boot-th, ghc-prim, hspec, hspec-discover
, QuickCheck, stdenv, tagged, template-haskell, th-abstraction
, transformers, transformers-compat
}:
mkDerivation {
  pname = "deriving-compat";
  version = "0.4.1";
  src = fetchgit {
    url = "https://github.com/haskell-compat/deriving-compat.git";
    sha256 = "1m1hfyrhkhgr25bp90082rrgh062dfyixx0q4j0f91r443qib4yy";
    rev = "ba03150271a664f7659ac824304453ecbaf353f4";
  };
  libraryHaskellDepends = [
    base containers ghc-boot-th ghc-prim template-haskell
    th-abstraction transformers transformers-compat
  ];
  testHaskellDepends = [
    base base-compat base-orphans hspec QuickCheck tagged
    template-haskell transformers transformers-compat
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell-compat/deriving-compat";
  description = "Backports of GHC deriving extensions";
  license = stdenv.lib.licenses.bsd3;
}
