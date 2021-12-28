#!/usr/bin/env python3
import sys
from collections import deque
from functools import reduce
from itertools import chain

# Represent numbers as a list of (value, depth) pairs.
# Inspired by the alternate solution in https://github.com/kfl/aoc_2021/blob/main/day18/day18.hs

def parse(l):
    d = 0
    res = []

    for c in l:
        if c == '[':   d += 1
        elif c == ']': d -= 1
        elif c == ',': pass
        else:          res.append( (int(c), d) )

    return res

def add(n1, n2):
    res = [ (v, d+1) for (v, d) in chain(n1, n2) ]

    while True:
        (res, changed) = explode(res)
        if changed: continue

        (res, changed) = split(res)
        if changed: continue

        break

    return res

def explode(n):
    for i in range(len(n)-1):
        if n[i][1] == 5:
            res = n.copy()
            if i > 0:        res[i-1] = (n[i-1][0] + n[i][0],   n[i-1][1])
            if i < len(n)-2: res[i+2] = (n[i+2][0] + n[i+1][0], n[i+2][1])
            res[i] = (0, 4)
            del res[i+1]

            return (res, True)

    return (n, False)


def split(n):
    for i in range(len(n)):
        res = n.copy()
        v = n[i][0]
        if v > 9:
            res[i] = (v // 2, n[i][1] + 1)
            res.insert(i+1, (v // 2 + v % 2, n[i][1] + 1))
            return (res, True)
    return (n, False)

def magnitude(n):
    q = deque(n)
    aq = []

    (av, ad) = q.popleft()
    while True:
        if len(q) == 0 and ad == 0: return av
        if len(aq) > 0 and aq[-1][1] == ad:
            (av, ad), (av2, ad2) = aq.pop(), (av, ad)
        else:
            (av2, ad2) = q.popleft()
        if ad == ad2:
            (av, ad) = (av*3 + av2*2, ad-1)
        else:
            aq.append( (av, ad) )
            (av, ad) = (av2, ad2)

def run():
    nums = [ parse(l.strip()) for l in sys.stdin ]

    p1 = reduce(add, nums)
    print("Part 1:", magnitude(p1))

    p2 = [ magnitude(add(nums[i], nums[j])) for i in range(len(nums)) for j in range(len(nums)) if i != j ]
    print("Part 2:", max(p2))



if __name__ == "__main__":
    run()
