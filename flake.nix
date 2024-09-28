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
        plwiki = pkgs.callPackage ./plwiki.nix { };
      in
      {
        packages.default = plwiki;
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = plwiki.nativeBuildInputs;
          buildInputs = with pkgs; [
            nodejs_22
            nodePackages.mathjax
            nodePackages.serve
          ];
        };
      }
    );
}
