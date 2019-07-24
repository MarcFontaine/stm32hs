{ mkDerivation, base, binary, bytestring, containers, stdenv
, STLinkUSB, STM32F103xx-SVD, transformers
}:
mkDerivation {
  pname = "STM32-Zombie";
  version = "0.2.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers STLinkUSB STM32F103xx-SVD
    transformers
  ];
  description = "control a STM32F103 microcontroller";
  license = stdenv.lib.licenses.bsd3;
}
