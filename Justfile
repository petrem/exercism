BOILERPLATE_DIR := justfile_directory() / "_boilerplate_templates"
NIX_DIR :=  justfile_directory() / "nix"

# By default, list available recipes
_default:
    @unjust --list

# Initialize a track with boilerplate files
init-track track backup='yes':
    #!/usr/bin/env bash
    set -euo pipefail
    backup_type={{if backup =~ 'n|N|no|No|NO|false|False|FALSE' {"off"} else {"numbered"} }}
    target={{justfile_directory() / track}}
    mkdir -p "$target"
    echo "Installing boilerplate files to $target"
    install --backup=$backup_type -m 644 {{BOILERPLATE_DIR / "shell.nix"}} "$target"
    install --backup=$backup_type -m 644 {{BOILERPLATE_DIR / "Justfile"}} "$target"
    install --backup=$backup_type -m 644 {{BOILERPLATE_DIR / "track.just"}} "$target"
    install --backup=$backup_type -m 644 {{BOILERPLATE_DIR / "dot-envrc"}} "$target"/.envrc
    direnv allow "$target"
    stub_nix={{NIX_DIR / "track-" + track + ".nix"}}
    echo "Creating stub Nix config for track: $stub_nix"
    install --backup=$backup_type -m 644 {{BOILERPLATE_DIR / "track.nix"}} "$stub_nix"

# Clean up backup files of various sorts
cleanup:
    find . \( -name \*~ -o -name \*.~* \) -delete
