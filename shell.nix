{ pkgs ? import ./pkgs.nix }:
let
  inherit (pkgs.haskellPackages) shellFor;
in
  shellFor {
    packages = p: [ (import ./. { inherit pkgs; }) ];
  }
