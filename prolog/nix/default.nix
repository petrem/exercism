{ pkgs ? import <nixpkgs> {} }:

with pkgs;

buildEnv {
  name = "builder";
  paths = [
    swiProlog
  ];
}
