{ pkgs ? import <nixpkgs> {} }:

let
  lyah-site = pkgs.haskellPackages.callCabal2nix "lyah-site" ./. {};
in
pkgs.mkShell {
  buildInputs = [
    lyah-site
    pkgs.cabal-install
  ];
}
