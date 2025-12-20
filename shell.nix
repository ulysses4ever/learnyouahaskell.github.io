{ pkgs ? import <nixpkgs> {} }:

let
  haskellEnv = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    hakyll
    pandoc
    pandoc-types
    text
    directory
  ]);
in
pkgs.mkShell {
  buildInputs = [
    haskellEnv
  ];
}
