{ mkDerivation, base, logict, stdenv }:
mkDerivation {
  pname = "setTalk";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base logict ];
  license = stdenv.lib.licenses.bsd3;
}
