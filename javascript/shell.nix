{ pkgs ?
  let sources = import ../nix/sources.nix;
  in import sources.nixpkgs {config.allowUnfree = true;}}:

with pkgs;
let
  inherit (lib) optional optionals;
in

mkShell {
  buildInputs = [
    (import ./nix/default.nix { inherit pkgs; })
    entr
    exercism
    git
    just
    jq 
  ]
  ++ optional stdenv.isLinux inotify-tools
  ++ optional stdenv.isDarwin terminal-notifier
  ++ optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    CoreFoundation
    CoreServices
  ]);
}
