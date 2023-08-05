## Nix

### Niv

Attempting to use [niv](https://github.com/nmattia/niv) to maintain the channel to NixOS
packages, "centrally", while still defining the environment for each track.

    nix-shell -p niv --run 'niv init'
    nix-shell -p niv --run 'niv modify nixpkgs --branch nixos-23.05'
    nix-shell -p niv --run 'niv update'

This creates `nix/sources.json` and `nix/sourcex.nix`. The latter is used in each
track's nix setup.

TODO: consider adding some generic tools via niv (like `exercism`, `just`), here at the
repo level. Maybe.

### Adding nix setup to a track

Create the `<track-directory>/nix/default.nix` and a `<track-directory>/shell.nix`. See
existing tracks for examples.


## Direnv

The nix shell is activated via `direnv`. To add to a new track, just do:

    echo "use_nix" > <track-directory>/.envrc
    cd <track-directory>
    direnv allow

## Justfiles

Just... add a `justfile` similar to existing ones.

TODO: serious cleanup of existing justfiles.

TODO: common library to factor out code. Wait for 'import' (was it 'inherit'?) to become
stable?
