with import <nixpkgs> {};

haskell.lib.buildStackProject {
  ghc = haskell.packages.ghc7103.ghc;
  name = "rayer";
  buildInputs = [ zlib libdevil ];
}
