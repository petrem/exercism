{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  inherit (lib) optional optionals;
in

buildEnv {
  name = "builder";
  paths = [
    gnumake
  ]
  ++ optional stdenv.isLinux valgrind;
}
