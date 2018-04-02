{ mkDerivation, base, bytestring, containers, fetchgit, fingertree
, mtl, parsers, prettyprinter, prettyprinter-ansi-terminal
, QuickCheck, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, transformers
}:
mkDerivation {
  pname = "parsix";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/ollef/parsix";
    sha256 = "13g0r8jd5iylyinzc7m11lykv3gvs91bq3209f1g9a4zaw910kxw";
    rev = "b0e0597ee0947181e40939ab7a77a3b6ba496b21";
  };
  libraryHaskellDepends = [
    base bytestring containers fingertree mtl parsers prettyprinter
    prettyprinter-ansi-terminal text transformers
  ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-hunit tasty-quickcheck text
  ];
  homepage = "https://github.com/ollef/parsix";
  license = stdenv.lib.licenses.bsd3;
}
