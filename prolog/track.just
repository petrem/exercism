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
    # TODO: would probably need to generate `-f file1 -f file2 ...` if there would be
    # more than one solution files. Similar of test files.
    swipl -f {{SOLUTION_FILES}} -s {{TEST_FILES}} -g run_tests,halt -t 'halt(1)' -- --all mix test --include pending

# Track-specific exercise upgrade steps
[no-cd]
_upgrade:
    # TODO: << track specific >>
