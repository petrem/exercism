#!/usr/bin/env bash

result=''
while read -rn 3 codon; do
    case "$codon" in
        AUG) result="$result Methionine" ;;
        UUU|UUC) result="$result Phenylalanine" ;;
        UUA|UUG) result="$result Leucine" ;;
        UCU|UCC|UCA|UCG) result="$result Serine" ;;
        UAU|UAC) result="$result Tyrosine" ;;
        UGU|UGC) result="$result Cysteine" ;;
        UGG) result="$result Tryptophan" ;;
        UAA|UAG|UGA|'') break ;;
        *) echo Invalid codon ; exit 1;;
    esac
done <<< "$1"
echo $result

