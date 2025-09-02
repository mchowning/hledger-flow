{
  description = "haskell configuration.";

  inputs = {
    # nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
    # nixpkgs.url = github:nixos/nixpkgs/release-24.11;
    nixpkgs.url = github:nixos/nixpkgs/release-25.05;
    utils.url = github:numtide/flake-utils;
  };
  outputs = { self, nixpkgs, utils, ... }:
  utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          #cabal-install
          stack
          #ghc
          #haskell.compiler.ghc964
          haskell.packages.ghc947.haskell-language-server
          #haskell-language-server
          #haskellPackages.hlint
          haskellPackages.hoogle

          zlib
          # Need zlib to get compilation to work. Some references:
          #   - https://github.com/commercialhaskell/stack/issues/2130#issuecomment-247032332
          #   - https://github.com/commercialhaskell/stack/issues/2975
        ];
        withHoogle = true;
      };
    }
  );
}
