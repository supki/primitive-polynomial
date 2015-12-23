{ mkDerivation, base, hspec, stdenv }:
mkDerivation {
  pname = "primitive-polynomial";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/supki/primitive-polynomial";
  description = "Primitive polynomial sequences";
  license = stdenv.lib.licenses.bsd2;
}
