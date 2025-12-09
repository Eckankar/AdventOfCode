#/bin/sh
stack build
cat input.txt | nice stack exec solve2 --rts-options "-N12"
