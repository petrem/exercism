#!/usr/bin/env bash

number=$1

while [ $number -ge 1000 ]; do   echo -n M;  number=$((number - 1000)); done
if    [ $number -ge  900 ]; then echo -n CM; number=$((number -  900)); fi
if    [ $number -ge  500 ]; then echo -n D;  number=$((number -  500)); fi
if    [ $number -ge  400 ]; then echo -n CD; number=$((number -  400)); fi

while [ $number -ge  100 ]; do   echo -n C;  number=$((number -  100)); done
if    [ $number -ge   90 ]; then echo -n XC; number=$((number -   90)); fi
if    [ $number -ge   50 ]; then echo -n L;  number=$((number -   50)); fi
if    [ $number -ge   40 ]; then echo -n XL; number=$((number -   40)); fi

while [ $number -ge   10 ]; do   echo -n X;  number=$((number -   10)); done
if    [ $number -ge    9 ]; then echo -n IX; number=$((number -    9)); fi
if    [ $number -ge    5 ]; then echo -n V;  number=$((number -    5)); fi
if    [ $number -ge    4 ]; then echo -n IV; number=$((number -    4)); fi

while [ $number -ge    1 ]; do   echo -n I;  number=$((number -    1)); done
echo
