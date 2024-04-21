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

# Search tracks for given exercise
find exercise:
    find . -maxdepth 2 -mindepth 2 -ipath "./*/{{exercise}}" | sort

missing exercise:
    #!/usr/bin/env python3
    from pathlib import Path
    print("\n".join(sorted(f.name for f in Path(".").iterdir() if f.is_dir() and (f / "track.just").exists() and not f.name.startswith("_") and not (f / "{{exercise}}").is_dir())))
