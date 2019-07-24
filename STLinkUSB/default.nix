{ compiler   ? "ghc865"
, haddock    ? false
, test       ? false
, benchmarks ? false
, pkgs       ? import <nixpkgs> {}
}:
with builtins;
let

lib         = pkgs.haskell.lib;
callPackage = pkgs.haskell.packages.${compiler}.callPackage;

doHaddock = if haddock
  then lib.doHaddock
  else lib.dontHaddock;
doTest = if test
  then lib.doCheck
  else lib.dontCheck;
doBench = if benchmarks
  then lib.doBenchmark
  else pkgs.lib.id;
in
{
stlinkusb=doHaddock(doTest(doBench( callPackage ./STLinkUSB.nix {} )));
}
