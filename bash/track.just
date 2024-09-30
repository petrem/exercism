# track-specific tasks

[no-cd]
_clean:
    # TODO: << track specific >>

[no-cd]
_lint:
    shellcheck {{SOLUTION_FILES}}


# Auto-format code files
[no-cd]
_style:
    # TODO: << track specific >>

# Run tests
[no-cd]
_test:
    BATS_RUN_SKIPPED=true bats *.bats

# Track-specific exercise upgrade steps
[no-cd]
_upgrade:
    # TODO: << track specific >>
