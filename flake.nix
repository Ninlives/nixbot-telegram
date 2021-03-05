{
  description = "A telegram bot that evaluate the nix expressions.";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        inherit (nixpkgs.legacyPackages.${system})
          haskell substituteAllFiles coreutils;
        inherit (haskell.lib) unmarkBroken doJailbreak dontCheck;
        inherit (nixpkgs.lib) sourceByRegex;
        hp = haskell.packages.ghc884;
      in rec {
        packages.nixbot-telegram = (hp.extend (final: prev: {
          telegram-raw-api = doJailbreak (unmarkBroken prev.telegram-raw-api);
          telegram-types = unmarkBroken prev.telegram-types;
        })).callCabal2nix "nixbot-telegram" (substituteAllFiles {
          timeout = "${coreutils}/bin/timeout";
          name = "source";
          src = sourceByRegex ./. [ "^src.*$" "^.*\\.cabal.*" ];
          files = [ "." ];
        }) { };
        defaultPackage = packages.nixbot-telegram;
        devShell = hp.shellFor { packages = p: [ defaultPackage ]; };
      });
}
