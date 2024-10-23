{
  description = "plwiki";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        plwiki-builder = pkgs.haskellPackages.callCabal2nix "plwiki-builder" ./. { };
        plwiki = pkgs.callPackage (
          {
            stdenv,
            lib,
            plwiki-builder,
            ...
          }:
          stdenv.mkDerivation {
            pname = "plwiki";
            version = "latest";
            src = lib.fileset.toSource {
              root = ./.;
              fileset = lib.fileset.unions [
                ./src
              ];
            };
            nativeBuildInputs = [ plwiki-builder ];
            LANG = "C.utf8";
            buildPhase = "build -j all";
            installPhase = ''
              mkdir -p $out
              cp -r ./site/* $out
            '';
          }
        ) { inherit plwiki-builder; };
      in
      {
        packages.plwiki-builder = plwiki-builder;
        packages.default = plwiki;
        devShells.default = pkgs.mkShell {
          inputsFrom = [ plwiki-builder.env ];
          packages = with pkgs; [
            stack
            nodejs_22
            nodePackages.mathjax
            nodePackages.serve
          ];
        };
      }
    );
}
