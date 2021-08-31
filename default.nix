{ mkDerivation, nix-filter, base, bytestring, bytestring-lexing, containers
, doctest, lens, stdenv, yaml-light
}:
mkDerivation {
  pname = "yaml-light-lens";
  version = "0.3.4";
  src = nix-filter {
    root = ./.;
    include = [
      "./README.md"
      "./yaml-light-lens.cabal"
      "./LICENSE"
      "./Setup.hs"
      "src"
      (nix-filter.inDirectory "src")
      (nix-filter.inDirectory "tests")
    ];
  };
  libraryHaskellDepends = [
    base bytestring bytestring-lexing containers lens yaml-light
  ];
  testHaskellDepends = [ base doctest ];
  description = "Lens interface to yaml-light";
  license = stdenv.lib.licenses.bsd3;
}
