export EXERCISE_DIR := invocation_directory()
TRACK := file_name(justfile_directory())

# Note these will be evaluated within the shell command expansion, not here
# (that's why it kinda works)
EXERCISE := "`jq --raw-output '.exercise' $EXERCISE_DIR/.exercism/metadata.json`"
SOLUTION_FILES := "`jq --raw-output '.files.solution | join(\" \")' $EXERCISE_DIR/.exercism/config.json`"
TEST_FILES := "`jq --raw-output '.files.test | join(\" \")' $EXERCISE_DIR/.exercism/config.json`"

# By default, list available recipes
_default:
    @unjust --list


[no-cd]
_clean_generic:
    # Generic cleaning ...
    rm -f *~


# Upgrade exercise in place. Will overwrite files!
[no-cd]
upgrade-exercise: && _upgrade
    #!/usr/bin/env bash
    echo "Donloading new version of the exercise ..."
    set -euo pipefail
    [ ! -e .exercism/metadata.json ] && echo "Config not found: .exercism/metadata.json" && exit 1
    download_opts=$(jq --raw-output '. as {track: $track, exercise: $exercise} | "--track \($track) --exercise \($exercise) --force"' .exercism/metadata.json)
    exercism download $download_opts
    git add .exercism

# Git Commit
[no-cd]
commit:
    # Adding to git
    git add .
    git commit -m "exercism:{{TRACK}}:{{EXERCISE}}}"

# Submit solution
[no-cd]
submit:
    # Submitting...
    exercism submit {{SOLUTION_FILES}}

# Style, lint, test, submit, commit and clean
[no-cd]
all: && style lint test submit commit clean
    # The Full Monty


# These are generic targets which should all have _<target> counterparts
# defined in `track.just`.


# Clean up
[no-cd]
clean: _clean_generic && _clean
    # Cleaning ...

# Run linters, static analysers
[no-cd]
lint: && _lint
    # Linting ...

# Auto-format code files
[no-cd]
style: && _style
    # Styling ...

# Run tests
[no-cd]
test: && _test
    # Testing ...
