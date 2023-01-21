#!/usr/bin/env bash

word=$(tr '[:upper:]' '[:lower:]' <<< "$1")
sum=0
for (( j=0; j<${#word}; j++ )); do
    case "${word:$j:1}" in
        'a'|'e'|'i'|'o'|'u'|'l'|'n'|'r'|'s'|'t') val=1 ;;
        'd'|'g') val=2 ;;
        'b'|'c'|'m'|'p') val=3 ;;
        'f'|'h'|'v'|'w'|'y') val=4 ;;
        'k') val=5 ;;
        'j'|'x') val=8 ;;
        'q'|'z') val=10 ;;
        *)
            echo bubu
            exit 1
            ;;
    esac
    sum=$((sum + val))
done
echo $sum
