# track-specific tasks

[no-cd]
_clean:
    cargo clean

[no-cd]
_lint:
    cargo check
    cargo clippy -- --no-deps -Dwarnings

# Auto-format code files
[no-cd]
_style:
    cargo fmt

# Run tests
[no-cd]
_test:
    cargo test -- --include-ignored

# Track-specific exercise upgrade steps
[no-cd]
_upgrade:
    # TODO: << track specific >>
