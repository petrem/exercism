#!/usr/bin/env bash

eggs="$1"
count=0

while [ "$eggs" -gt 0 ]; do
    count=$((count + (eggs & 1)))
    eggs=$((eggs >> 1))
done
echo $count
