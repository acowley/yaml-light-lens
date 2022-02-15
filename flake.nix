{
  description = "Lens interface to yaml-light.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system: let
      compiler = "8107";
      # compiler = "921";
      pkgs = import nixpkgs {
        inherit system;
      };
      dontCheck = pkgs.haskell.lib.dontCheck;
      dontHaddock = pkgs.haskell.lib.dontHaddock;
      hspkgs = (pkgs.haskell.packages."ghc${compiler}").override {
        overrides = self: super: 
          if compiler == "921"
          then { 
            # QuickCheck = dontCheck super.QuickCheck;
            # optparse-applicative = dontCheck super.optparse-applicative;
            # generic-deriving = dontHaddock super.generic-deriving;
          } 
          else { };
      };
      drv = hspkgs.callPackage ./default.nix {
        nix-filter = nix-filter.lib;
      };
      ghc = hspkgs.ghc.withPackages (ps: 
        drv.passthru.getBuildInputs.haskellBuildInputs
      );
  in {
    devShell = pkgs.mkShell {
      buildInputs = [
        ghc
        hspkgs.cabal-install
      ];
    };
  });
}
