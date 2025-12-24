{ pkgs ? import <nixpkgs> {} }:

let
  # Build the Haskell site generator executable
  siteGenerator = pkgs.haskellPackages.callCabal2nix "lyah-site" ./. {};
in
pkgs.stdenv.mkDerivation {
  name = "learnyouahaskell-site";
  src = ./.;
  
  buildInputs = [ siteGenerator ];
  
  buildPhase = ''
    # Run the site generator to build the static site
    ${siteGenerator}/bin/site build
  '';
  
  installPhase = ''
    # Copy the generated site to the output directory
    mkdir -p $out
    cp -r _site/* $out/
  '';
}
