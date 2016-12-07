{ mkDerivation, base, old-time, parsec, stdenv }:
mkDerivation {
  pname = "UMM";
  version = "0.2.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base old-time parsec ];
  homepage = "http://www.korgwal.com/umm/";
  description = "A small command-line accounting tool";
  license = "GPL";
}
