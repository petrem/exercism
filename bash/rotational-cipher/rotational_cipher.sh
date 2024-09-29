#!/usr/bin/env bash

( tr A-Z $(head -c$(($2 % 26 + 1)) <<< ABCDEFGHIJKLMNOPQRSTUVWXYZ | tail -c1)-ZA-Z | tr a-z $(head -c$(($2 % 26 + 1)) <<< abcdefghijklmnopqrstuvwxyz | tail -c1)-za-z ) <<< $1
