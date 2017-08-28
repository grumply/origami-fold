{ mkDerivation, base, containers, deepseq, stdenv, trivial }:
mkDerivation {
  pname = "origami-fold";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  executableHaskellDepends = [
    base containers deepseq trivial
  ];
  license = stdenv.lib.licenses.bsd3;
}
