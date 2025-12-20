{ pkgs ? import <nixpkgs> {} }:

let
  myPackage = pkgs.haskellPackages.callCabal2nix "lyah-site" ./. {};
in
pkgs.haskellPackages.shellFor {
  packages = p: [ myPackage ];
  buildInputs = with pkgs; [
    haskellPackages.cabal-install
  ];
}
