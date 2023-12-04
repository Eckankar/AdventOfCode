#!/usr/bin/env python

import re
import sys

def scoreCard1(cardid, winning, actual):
    overlap = winning & actual

    if not overlap: return 0
    return 2 ** (len(overlap)-1)

def scoreCard2(cardid, winning, actual, scorelog):
    overlap = winning & actual

    return 1 + sum( scorelog[-i] for i in range(1,len(overlap)+1) )

cards = []
for line in sys.stdin:
    (cardid, winning, actual) = re.match(r'Card\s+(\d+):\s+([^|]*)\s+\|\s+(.*)\s*$', line).groups()
    winning = set( winning.split() )
    actual  = set( actual.split() )

    cards.append( (cardid, winning, actual) )

score1 = 0
score2log = []

for card in reversed(cards):
    score1 += scoreCard1(*card)

    score2log.append( scoreCard2(*card, score2log) )

score2 = sum(score2log)

print(f"Part 1: {score1}")
print(f"Part 2: {score2}")
