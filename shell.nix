{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  inherit (lib) optional optionals;
in

mkShell {
  buildInputs = [
    entr
    exercism
    git
    jq
    just
  ] ++ optional stdenv.isLinux inotify-tools
  ++ optional stdenv.isDarwin terminal-notifier
  ++ optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    CoreFoundation
    CoreServices
  ]);
}
