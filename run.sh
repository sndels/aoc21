#!/bin/bash
if [ ! -d bin ]; then
    mkdir bin
fi

stack build && stack exec aoc21 $1 $2

