{
  description = "plwiki";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      defaultPackage.x86_64-linux = pkgs.callPackage ./plwiki.nix { };
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = with pkgs; [
          (ghc.withPackages (
            hpkgs: with hpkgs; [
              pandoc
              blaze-html
              shakespeare
              optparse-applicative
            ]
          ))
          nodejs_22
          nodePackages.mathjax
          nodePackages.serve
        ];
      };
    };
}
