{
  description = "Lens interface to yaml-light";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    hls.url = "github:haskell/haskell-language-server";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter, ... }@inputs:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      compiler = "ghc8104";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; allowBroken = true; };
      };
      hspkgs = pkgs.haskell.packages.${compiler};
      yaml-light-lens = hspkgs.callPackage ./default.nix { nix-filter = nix-filter.lib; };
      # ghc = hspkgs.ghc.withHoogle (ps: yaml-light-lens.passthru.getBuildInputs.haskellBuildInputs);
      ghc = hspkgs.ghc.withHoogle (ps: [ps.base
                                        ps.bytestring
                                        ps.bytestring-lexing
                                        ps.containers
                                        ps.doctest
                                        ps.lens
                                        ps.yaml-light]);
  in {
    devShell = pkgs.mkShell {
      buildInputs = [
        ghc
        hspkgs.cabal-install
        hspkgs.haskell-language-server
      ];
      shellHook = ''
        export NIX_GHC=${ghc}/bin/ghc
      '';
    };
    packages.yaml-light-lens = yaml-light-lens;
    defaultPackage = self.packages.${system}.yaml-light-lens;
  });
}
