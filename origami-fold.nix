{ mkDerivation, base, containers, deepseq, stdenv, ghc }:
mkDerivation {
  pname = "origami-fold";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers deepseq ghc ];
  license = stdenv.lib.licenses.bsd3;
}
