{ stdenv, ghc, ... }:
let
  haskellPackages =
    hpkgs: with hpkgs; [
      pandoc
      blaze-html
      shakespeare
      optparse-applicative
      shake
    ];
in
stdenv.mkDerivation {
  pname = "plwiki";
  version = "latest";
  src = ./.;
  buildInputs = [ (ghc.withPackages haskellPackages) ];
  LANG = "C.utf8";
}
