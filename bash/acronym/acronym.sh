#!/usr/bin/env bash

set -euo pipefail

echo "$1" | \
    tr -c "[:alpha:]'" ' ' | \
    tr -s ' ' | \
    (
        while read -r -d ' ' word ; do
            head -c1 <<< "$word"
        done
    ) | \
    tr '[:lower:]' '[:upper:]'
