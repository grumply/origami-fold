{ mkDerivation, base, containers, deepseq, stdenv, trivial }:
mkDerivation {
  pname = "origami-fold";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  testHaskellDepends = [ base containers deepseq trivial ];
  benchmarkHaskellDepends = [ base containers deepseq trivial ];
  license = stdenv.lib.licenses.bsd3;
}
