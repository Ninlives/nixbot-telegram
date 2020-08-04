{ pkgs ? import ./pkgs.nix }:
let
  inherit (pkgs) haskellPackages;
  inherit (pkgs.haskell.lib) unmarkBroken doJailbreak dontCheck;
  inherit (pkgs.lib) sourceByRegex;
in
(haskellPackages.extend(self: super: {
  telegram-raw-api = doJailbreak (unmarkBroken super.telegram-raw-api);
  telegram-types = dontCheck (unmarkBroken super.telegram-types);
})).callCabal2nix 
  "nixbot-telegram" (sourceByRegex ./. [ 
    "^src.*$"
    "^.*\\.cabal.*"
  ]) {}
