{
  mkDerivation,
  base,
  stdenv,
}:
mkDerivation {
  pname = "STM32F401-SVD";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ base ];
  description = "Definition for peripherals, registers and Fields from STM32F401xx.svd";
  license = stdenv.lib.licenses.bsd3;
}
