{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ haskellNix.overlay
                     (final: prev: {
                       # This overlay adds our project to pkgs
                       firebase-auth =
                         final.haskell-nix.project' {
                           src = ./.;
                           compiler-nix-name = "ghc884";
                           shell.tools = {
                             cabal = {};
                             haskell-language-server = {};
                             hspec-discover = {};
                           };
                         };
                     })
                   ];
        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.firebase-auth.flake {};
      in flake // {
        defaultPackage = flake.packages."firebase-auth-jwt:lib";
      });
}
