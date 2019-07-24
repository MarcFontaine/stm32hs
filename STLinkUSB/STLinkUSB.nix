{ mkDerivation, base, binary, bytestring, stdenv, transformers, usb
, vector
}:
mkDerivation {
  pname = "STLinkUSB";
  version = "0.1.2";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring transformers usb vector
  ];
  homepage = "https://github.com/MarcFontaine/stm32hs";
  description = "STLink USB interface in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
