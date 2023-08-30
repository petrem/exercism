# track-specific tasks

[no-cd]
_clean:
    # TODO: << track specific >>

[no-cd]
_lint:
    # TODO: << track specific >>


# Auto-format code files
[no-cd]
_style:
    # TODO: << track specific >>

# Run tests
[no-cd]
_test:
    ## TODO: should transform TEST_FILES into a list of -l file1 -l file2 ...
    ## but likely there will always be one single file in the exercise
    emacs -q -batch -l ert -l {{TEST_FILES}} -f ert-run-tests-batch-and-exit

# Track-specific exercise upgrade steps
[no-cd]
_upgrade:
    # TODO: << track specific >>
