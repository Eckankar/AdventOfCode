#!/usr/bin/env python
import sys

from collections import defaultdict
from enum import IntEnum
from functools import cache
from itertools import product

Hand = IntEnum('Hand', ['HighCard', 'OnePair', 'TwoPair', 'ThreeKind', 'FullHouse', 'FourKind', 'FiveKind'])

@cache
def cardRank(c):
    return "23456789TJQKA".index(c)

@cache
def cardRank2(c):
    return "J23456789TQKA".index(c)

def removeJacks(hand):
    return product(*("AKQT98765432" if c == 'J' else [c] for c in hand))

def handType(hand, jacksWild=False):
    cardCount = defaultdict(int)
    for c in hand:
        cardCount[c] += 1

    if jacksWild and 'J' in cardCount:
        jackCount = cardCount['J']
        del cardCount['J']
        vs = sorted( cardCount.values() )

        match cardCount['J']:
            case 5: return Hand.FiveKind
            case 4: return Hand.FiveKind
            case 3: return (FiveKind if vs == [2] else FourKind)
            case _: return max( handType(hand2, jacksWild=False) for hand2 in removeJacks(hand) )

    vs = sorted( cardCount.values() )
    match vs:
        case [5]:         return Hand.FiveKind
        case [1,4]:       return Hand.FourKind
        case [2,3]:       return Hand.FullHouse
        case [1,1,3]:     return Hand.ThreeKind
        case [1,2,2]:     return Hand.TwoPair
        case [1,1,1,2]:   return Hand.OnePair
        case [1,1,1,1,1]: return Hand.HighCard

def parseLine(line):
    [cards, score] = line.split(' ')
    return (cards, int(score))

def main():
    data = [ parseLine(line.strip()) for line in sys.stdin ]

    data.sort(key=lambda hb: (handType(hb[0], jacksWild=False), list(map(cardRank, hb[0]))))
    score1 = sum(b*(i+1) for (i, (_, b)) in enumerate(data))
    print(f'Part 1: {score1}')

    data.sort(key=lambda hb: (handType(hb[0], jacksWild=True), list(map(cardRank2, hb[0]))))
    score2 = sum(b*(i+1) for (i, (_, b)) in enumerate(data))
    print(f'Part 2: {score2}')

if __name__ == '__main__':
    main()
