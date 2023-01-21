#!/usr/bin/env bash

if [ $# -ne 1 ] || ! grep -q -E '^[0-9]+$' <<< "$1" ; then
    echo "Usage: $0 <year>"
    exit 1
fi

(($1 % 4 == 0 && $1 % 100 != 0 || $1 % 400 == 0)) && echo true || echo false
