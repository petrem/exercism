# track-specific tasks

[no-cd]
_clean:
    make clean

[no-cd]
_lint:
    cppcheck {{SOLUTION_FILES}}


# Auto-format code files
[no-cd]
_style:
    # TODO: << track specific >>

# Run tests
[no-cd]
_test:
    make test

# Track-specific exercise upgrade steps
[no-cd]
_upgrade:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Move src/* to base dir to update to new exercism expectations"
    if [ -e src ]; then
        echo "Git-moving src/* to exercise directory ..."
        git mv -f src/*.{h,c} .
        rmdir src  # will fail if src/ is not empty
    fi
    echo "Removing TEST_IGNORE() from tests ..."
    sed -i -e '/TEST_IGNORE/d' {{TEST_FILES}}
    git add {{TEST_FILES}}
    if [ -e test ]; then
        echo "Removing old test dir 'test'"
        git rm -rf test/
    fi 
