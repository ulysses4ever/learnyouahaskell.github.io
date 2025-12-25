{ pkgs ? import <nixpkgs> {} }:

# Build the Haskell site generator executable
pkgs.haskellPackages.callCabal2nix "lyah-site" ./. {}
