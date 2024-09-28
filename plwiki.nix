{ stdenv, ghc, lib, ... }:
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
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./src
      ./translator
      ./Makefile
      ./build.hs
    ];
  };
  nativeBuildInputs = [ (ghc.withPackages haskellPackages) ];
  LANG = "C.utf8";
}
