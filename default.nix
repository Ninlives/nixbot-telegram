{ pkgs ? import ./pkgs.nix }:
let
  inherit (pkgs) haskellPackages substituteAllFiles coreutils;
  inherit (pkgs.haskell.lib) unmarkBroken doJailbreak dontCheck;
  inherit (pkgs.lib) sourceByRegex;
in (haskellPackages.extend (self: super: {
  telegram-raw-api = doJailbreak (unmarkBroken super.telegram-raw-api);
  telegram-types = dontCheck (unmarkBroken super.telegram-types);
})).callCabal2nix "nixbot-telegram" (substituteAllFiles {
  timeout = "${coreutils}/bin/timeout";
  name = "source";
  src = sourceByRegex ./. [ "^src.*$" "^.*\\.cabal.*" ];
  files = [ "." ];
}) { }
