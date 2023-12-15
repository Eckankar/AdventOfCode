#!/usr/bin/env python
import re
from itertools import count
from collections import defaultdict, OrderedDict

def hash(s):
    hv = 0
    for c in s:
        hv = (hv + ord(c)) * 17 % 256
    return hv

def initialize(data):
    m = defaultdict(OrderedDict)

    for d in data:
        (letters, op, val) = re.fullmatch(r'(.*)([-=])(.*)', d).groups()

        hv = hash(letters)
        if op == '=':
            m[hv][letters] = val
        elif op == '-':
            if hv in m and letters in m[hv]:
                del m[hv][letters]

    return m

def focusingPower(box):
    return sum( int(v) * n for ((_, v), n) in zip(box.items(), count(1)) )

data = input().split(',')

part1 = sum( hash(s) for s in data )
part2 = sum( focusingPower(box) * (hv + 1) for (hv, box) in initialize(data).items() )

print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
