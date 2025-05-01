export EXTRA_CONFIG_FILE := ".exercism/extra-files.json"

TRACK_DIR := justfile_directory()
export EXERCISE_DIR := invocation_directory()
TRACK := file_name(justfile_directory())

# Note these will be evaluated within the shell command expansion, not here
# (that's why it kinda works)
EXERCISE := "`jq --raw-output '.exercise' $EXERCISE_DIR/.exercism/metadata.json`"
CONFIG_SOLUTION_FILES := "`jq --raw-output '.files.solution | join(\" \")' $EXERCISE_DIR/.exercism/config.json`"
EXTRA_SOLUTION_FILES := if path_exists(EXERCISE_DIR / EXTRA_CONFIG_FILE) == "true"  {"`jq --raw-output '.files.solution | join(\" \")' $EXERCISE_DIR/$EXTRA_CONFIG_FILE`"} else {""}
SOLUTION_FILES := CONFIG_SOLUTION_FILES + " " + EXTRA_SOLUTION_FILES
TEST_FILES := "`jq --raw-output '.files.test | join(\" \")' $EXERCISE_DIR/.exercism/config.json`"

# By default, list available recipes
_default:
    @unjust --list



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

[no-cd]
_clean_generic:
    # Generic cleaning ...
    rm -f *~


# Common target definitions

# Upgrade exercise in place. Will overwrite files!
[no-cd]
upgrade-exercise: && _upgrade
    #!/usr/bin/env bash
    echo "Downloading new version of the exercise ..."
    set -euo pipefail
    [ ! -e .exercism/metadata.json ] && echo "Config not found: .exercism/metadata.json" && exit 1
    download_opts=$(jq --raw-output '. as {track: $track, exercise: $exercise} | "--track \($track) --exercise \($exercise) --force"' .exercism/metadata.json)
    exercism download $download_opts
    git add .exercism

# Download exercise
[no-cd]
get-exercise exercise: && (_get-exercise exercise)
    #!/usr/bin/env bash
    echo "Downloading {{TRACK}}/{{exercise}} ..."
    exercism download --track={{TRACK}} --exercise={{exercise}}
    touch {{TRACK_DIR}}/{{exercise}}/.projectile

# Git Commit
[no-cd]
commit:
    # Adding to git
    git add .
    git commit -m "exercism:{{TRACK}}:{{EXERCISE}}"

# Submit solution
[no-cd]
submit:
    # Submitting...
    exercism submit {{SOLUTION_FILES}}

# Initialise, style, lint, test, commit, submit and clean
[no-cd]
all: && _init style lint test commit submit clean
    # The Full Monty


[no-cd]
_create-extra-config:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ ! -e {{EXTRA_CONFIG_FILE}} ] ; then
      cat << EOT > {{EXTRA_CONFIG_FILE}}
    {
      "files": {
        "solution": [
        ],
        "test": [
        ]
      }
    }
    EOT
    fi

# Append an extra solution source file. The "kind" argument can be
# "solution" (the default) or "test".
[no-cd]
add-src path kind='solution': _create-extra-config
    #!/usr/bin/env bash
    set -euo pipefail
    jq '.files.{{kind}} |= . + ["{{path}}"]' {{EXTRA_CONFIG_FILE}} > {{EXTRA_CONFIG_FILE + ".tmp"}}
    mv {{EXTRA_CONFIG_FILE + ".tmp"}} {{EXTRA_CONFIG_FILE}}

