# track-specific tasks

[no-cd]
_clean:
    find . -name node_modules -type d -prune -exec rm -rf \{\} +

[no-cd]
_lint:
    # TODO: is there linting wasm?


# Auto-format code files
[no-cd]
_style:
    # TODO: << track specific >>

# Run tests
[no-cd]
_test:
    npm install && npm run test

# Track-specific exercise upgrade steps
[no-cd]
_upgrade:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Replacing xtest with test in spec(s) ..."
    sed -i -e '/^ *xtest/s/xtest/test/' {{TEST_FILES}}
    git add {{TEST_FILES}}


# Run after getting exercise
[no-cd]
_get-exercise exercise:
    # TODO: << track specific >>
