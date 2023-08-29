# A wrapper for `just` that just adds the `--unstable` argument.
# This is a workaround for not being able to create bash aliases via
# `mkShell`'s `shellHook`.

{pkgs ? import <nixpkgs> {}}:

with pkgs;

let
  name = "unjust";
  unjust-script = writeShellScriptBin name ''
    just --unstable "$@"
  '';
in
symlinkJoin {
  inherit name;
  paths = [ just unjust-script ];
  buildInputs = [makeWrapper];
  postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
}
  

  # See
  # https://www.ertt.ca/nix/shell-scripts/
  # https://nixos.org/manual/nixpkgs/stable/#ssec-stdenv-functions
