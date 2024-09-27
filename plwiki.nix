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
  nativeBuildInputs = [ (ghc.withPackages haskellPackages) ];
  LANG = "C.utf8";
}
