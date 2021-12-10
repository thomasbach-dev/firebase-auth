{
  description = "A very basic flake";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix/3aa5ffc0afef728894a08c33a06029997cfd3872";
    flake-utils.url = "github:numtide/flake-utils/74f7e4319258e287b0f9cb95426c9853b282730b";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };
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
