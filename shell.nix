{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.callCabal2nix "lyah-site" ./. {})
  ];
}
