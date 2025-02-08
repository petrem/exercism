# track-specific tasks

# Haskell track includes the `package.yaml` in the solutions file, which is good
# for submitting but not for e.g. linting.
SRC_FILES := "`jq --raw-output '[ .files.solution | .[] | match(\"^.*[.]hs$\").string ] | join(\" \")' $EXERCISE_DIR/.exercism/config.json` " + EXTRA_SOLUTION_FILES
#EXTRA_SRC_FILES :=

[no-cd]
_clean:
    stack-clean-old delete-work -d -r -y

[no-cd]
_lint:
    # TODO: decide if I'm pleased with this
    #hindent --validate --sort-imports --line-length 88 {{SRC_FILES}}
    ormolu --mode check {{SRC_FILES}}
    hlint {{SRC_FILES}}

# Auto-format code files
[no-cd]
_style:
    # TODO: decide if I want auto-formatting here
    #hindent --sort-imports --line-length 88 {{SRC_FILES}}
    ormolu --mode inplace {{SRC_FILES}}

# Run tests
[no-cd]
_test:
    stack test

# Track-specific exercise upgrade steps
[no-cd]
_upgrade:
    # TODO: << track specific >>
