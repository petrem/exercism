#!/usr/bin/env bash

if grep -q '[^GCTA]' <<< "$1"; then
    echo "Invalid nucleotide detected."
    exit 1
fi
tr GCTA CGAU <<< "$1"
