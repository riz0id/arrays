{ mkDerivation, base, fetchgit, lib, template-haskell }:
mkDerivation {
  pname = "array-exceptions";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/array-exceptions";
    sha256 = "14rf98by3lzfnadsrkb2a0p0xm7c80lql1dcpl12mh9ck45x675s";
    rev = "af07def10cf03065fa48827d283bc5820a1c5f84";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base template-haskell ];
  homepage = "https://github.com/riz0id/array-exceptions";
  description = "TODO";
  license = lib.licenses.isc;
}
