{ pkgs ? import <nixpkgs> {} }:

with pkgs;

buildEnv {
  name = "builder";
  paths = [
    erlang
    rebar3
  ];
}
