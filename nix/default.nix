let pkgsOptions = {
      config.allowUnfree = true;
      config.permittedInsecurePackages = [
        # this is added for swiProlog in the prolog track
        # to be retested later, e.g. in 23.11 or something
        "openssl-1.1.1v"
      ];
    };
in
{ pkgs ?
  let sources = import ./sources.nix;
  in import sources.nixpkgs pkgsOptions
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
