{ mkDerivation, base, old-time, parsec, stdenv, utf8-string }:
mkDerivation {
  pname = "UMM";
  version = "0.2.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base old-time parsec utf8-string ];
  homepage = "http://www.korgwal.com/umm/";
  description = "A small command-line accounting tool";
  license = "GPL";
}
