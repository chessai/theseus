{ package ? "theseus", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).theseus
