#!/usr/bin/env bash

input=$(echo "$1" | tr -d -c '[:alpha:]' |tr '[:upper:]' '[:lower:]')
for c in a b c d e f g h i j k l m n o p q r s t u v w x y z ; do
    if ! grep $c <<< "$input" >/dev/null; then
        echo false
        exit
    fi
done

echo true
