{ nixpkgs ? import <nixpkgs> {} }:
let
  
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    gloss
    stylish-haskell
    hindent
    hlint
    ghcid
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    pkgs.gdb
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
  ];
in
pkgs.stdenv.mkDerivation {
  name = "haskell-cube";
  buildInputs = nixPackages;
}
