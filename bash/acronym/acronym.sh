#!/usr/bin/env bash

echo "$1" | \
    tr -c "a-zA-Z'" ' ' | \
    tr -s ' ' | \
    (
        while read -d ' ' word ; do
            head -c1 <<< "$word"
        done
    ) | \
    tr a-z A-Z
