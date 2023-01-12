#!/usr/bin/env bash

number=$1
raindrops=''

append () {
    raindrops="$raindrops$1"
}

[ $(( $number % 3 )) -eq 0 ] && append "Pling"
[ $(( $number % 5 )) -eq 0 ] && append "Plang"
[ $(( $number % 7 )) -eq 0 ] && append "Plong"

[ -n "$raindrops" ] && echo "$raindrops" || echo "$number"
