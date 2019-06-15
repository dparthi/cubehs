let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "19.03";
    sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
  };
  pkgs = import nixpkgs {};
in
pkgs.stdenv.mkDerivation {
  name = "haskell-cube";
  buildInputs = [
    (pkgs.haskell.packages.ghc864.ghcWithHoogle (hps:
      with hps;
      [
        # libraries
        ghcid
        stylish-haskell
        hindent
        hlint
        cabal-install
        gloss
      ]))
    pkgs.haskellPackages.cabal2nix
  ];
}
