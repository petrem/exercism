## Nix

### Niv

Attempting to use [niv](https://github.com/nmattia/niv) to maintain the channel to NixOS
packages, "centrally", while still defining the environment for each track.

    nix-shell -p niv --run 'niv init'
    nix-shell -p niv --run 'niv modify nixpkgs --branch nixos-23.05'
    nix-shell -p niv --run 'niv update'

This creates `nix/sources.json` and `nix/sourcex.nix`. The latter is used in each
track's nix setup.

### Top level nix/just

* Nix and just setup common for all tracks, including the `exercism` CLI and some other
  tools.
* A nix package to wrap `just --unstable` into an executable called `unjust` (because
  due to how `direnv` works -- it injects environment variables into the current shell,
  rather than sourcing files --, we can't initialize a shell alias from the nix
  `mkShell`.
* Template files for track-specific nix & just boilerplate.

### Setting up a track

From the top level directory, run `unjust init-track <track>`.

This will

1. Add generic stubs `<track>/Justfile` and `<track>/shell.nix`. They should not be
   edited.
2. Add `<track>/.envrc` and run `direnv allow` on it.
3. Copy templates for track-specific nix and just files. See below.
4. Backup any existing files, by default (`init-track <track> no` will prevent this).

Then edit these two files:
* `nix/track-<track>.nix` and add the packages needed for the dev environment
* `<track>/track.just` and add track specific `just` recipes
