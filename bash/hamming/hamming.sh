#!/usr/bin/env bash

strand1="$1"
strand2="$2"

[ $# -ne 2 ] && echo "Usage: hamming.sh <string1> <string2>" && exit 1
[ $(wc -m <<< "$strand1") -ne $(wc -m <<< "$strand2") ] \
    && echo strands must be of equal length && exit 1
count=0
while [ -n "$strand1" ]; do
    [ "$(head -c1 <<< "$strand1")" != "$(head -c1 <<< "$strand2")" ] && count=$(($count + 1))
    strand1=$(tail -c +2 <<< "$strand1")
    strand2=$(tail -c +2 <<< "$strand2")    
done
echo $count
