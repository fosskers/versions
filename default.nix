{ mkDerivation, base, checkers, deepseq, hashable, megaparsec
, microlens, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "versions";
  version = "3.3.1";
  src = ./.;
  libraryHaskellDepends = [ base deepseq hashable megaparsec text ];
  testHaskellDepends = [
    base checkers microlens QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "Types and parsers for software version numbers";
  license = stdenv.lib.licenses.bsd3;
}
