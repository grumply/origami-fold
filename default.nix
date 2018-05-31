{ mkDerivation, base, containers, deepseq, stdenv }:
mkDerivation {
  pname = "origami-fold";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  license = stdenv.lib.licenses.bsd3;
}
