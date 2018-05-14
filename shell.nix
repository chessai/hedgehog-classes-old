{ package ? "hedgehog-classes", compiler ? "ghc841" }:

(import ./default.nix {
  inherit package compiler;
}).hedgehog-classes
