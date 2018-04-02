{ mkDerivation, base, bound, constrictor, containers, mtl, parsec
, parsix, prettyprinter, prettyprinter-ansi-terminal, stdenv
, transformers, vector
}:
mkDerivation {
  pname = "theseus";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bound constrictor containers mtl parsec parsix prettyprinter
    prettyprinter-ansi-terminal transformers vector
  ];
  homepage = "https://github.com/chessai/theseus.git";
  description = "lang";
  license = stdenv.lib.licenses.mit;
}
