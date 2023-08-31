{ pkgs ?
  let sources = import ./sources.nix;
  in import sources.nixpkgs {config.allowUnfree = true;}
, track ? null
}:

with pkgs;

let exercism = (import ./exercism.nix) pkgs;
    track_pkgs = if isNull track
                 then {build_pkgs = []; shell_pkgs = [];}
                 else import (./. + "/track-${track}.nix") pkgs;
    build = exercism.build_pkgs ++ track_pkgs.build_pkgs;
in {
  inherit build;
  shell = mkShell {
    inputsFrom = build;
    packages = exercism.shell_pkgs ++ track_pkgs.shell_pkgs;
  };
}
