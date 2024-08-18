#!/usr/bin/env bash

number=$1
roman=""

while [ $number -ge 1000 ]; do   roman="${roman}M";  number=$((number - 1000)); done
if    [ $number -ge  900 ]; then roman="${roman}CM"; number=$((number -  900)); fi
if    [ $number -ge  500 ]; then roman="${roman}D";  number=$((number -  500)); fi
if    [ $number -ge  400 ]; then roman="${roman}CD"; number=$((number -  400)); fi

while [ $number -ge  100 ]; do   roman="${roman}C";  number=$((number -  100)); done
if    [ $number -ge   90 ]; then roman="${roman}XC"; number=$((number -   90)); fi
if    [ $number -ge   50 ]; then roman="${roman}L";  number=$((number -   50)); fi
if    [ $number -ge   40 ]; then roman="${roman}XL"; number=$((number -   40)); fi

while [ $number -ge   10 ]; do   roman="${roman}X";  number=$((number -   10)); done
if    [ $number -ge    9 ]; then roman="${roman}IX"; number=$((number -    9)); fi
if    [ $number -ge    5 ]; then roman="${roman}V";  number=$((number -    5)); fi
if    [ $number -ge    4 ]; then roman="${roman}IV"; number=$((number -    4)); fi

while [ $number -ge    1 ]; do   roman="${roman}I";  number=$((number -    1)); done

echo $roman
