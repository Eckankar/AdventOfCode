#!/usr/bin/env python

import re
import sys
from functools import reduce
from itertools import groupby

lines = [ l.strip() for l in sys.stdin ]
seeds = [ int(v) for v in lines[0][6:].replace("seeds: ", "").split() ]
lines = [ list(v) for (b, v) in groupby(lines[2:], key=lambda e: len(e) > 0) if b ]

def compose(f, g):
    return lambda x, f=f, g=g: g( f(x) )

lastTo = "seed"
maps = []
for mlines in lines:
    (fromName, toName) = re.match(r'(\w+)-to-(\w+) map:$', mlines[0]).groups()
    if not lastTo == fromName: raise RuntimeError("expecting maps to be in order")
    lastTo = toName

    f = lambda x: x
    for line in mlines[1:]:
        (toStart, fromStart, rangeLen) = (int(v) for v in line.split())

        nf = lambda x, f=f, toStart=toStart, fromStart=fromStart, rangeLen=rangeLen: (x + (toStart - fromStart)) if x >= fromStart and x < fromStart + rangeLen else f(x)
        f = nf

    maps.append(f)

if not toName == "location": raise RuntimeError("expecting location as last map")

superF = reduce(compose, maps)

print( min([superF(s) for s in seeds]) )
