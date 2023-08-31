# track-specific tasks

[no-cd]
_clean:
    find . -name \*.pyc -delete
    find . \( -name __pycache__  -o -name .pytest_cache \) -type d -prune -exec rm -rf \{\} \;

[no-cd]
_lint:
    ruff check {{SOLUTION_FILES}}
    mypy {{SOLUTION_FILES}}
    pylint {{SOLUTION_FILES}}


# Auto-format code files
[no-cd]
_style:
    black {{SOLUTION_FILES}}
    # using a custom config file instead of default `.isort.cfg` to make it
    # more visible, that's all.
    isort --settings-file {{TRACK_DIR / "isort.cfg"}} {{SOLUTION_FILES}}

# Run tests
[no-cd]
_test:
    # TODO: << track specific >>

# Track-specific exercise upgrade steps
[no-cd]
_upgrade:
    # TODO: << track specific >>
