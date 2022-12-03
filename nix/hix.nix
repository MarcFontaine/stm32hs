{pkgs, ...}: {
  # name = "project-name";
  compiler-nix-name = "ghc8107"; # Version of GHC to use

  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 (
  pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    p.musl64
  ]);

  shell.withHoogle = false;
  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  
  # shell.tools.hlint = "latest";
  # shell.tools.haskell-language-server = "latest";
}
