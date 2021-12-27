#!/usr/bin/env python3
import heapq
import sys

def neighbors(y, x, h, w):
    return [ (yy, xx) for (yy, xx) in [ (y-1, x), (y+1, x), (y, x-1), (y, x+1) ]
                       if xx >= 0 and xx < w and yy >= 0 and yy < h ]

def solve(m):
    dm = [ [ float('inf') for p in r ] for r in m ]
    h, w = len(m), len(m[0])

    pq = []
    heapq.heappush(pq, (0, (0,0)))

    while len(pq) > 0:
        (cost, (y, x)) = heapq.heappop(pq)

        if dm[y][x] <= cost: continue

        dm[y][x] = cost
        for (yy, xx) in neighbors(y, x, h, w):
            newCost = cost + m[yy][xx]
            if newCost >= dm[yy][xx]: continue
            heapq.heappush(pq, (newCost, (yy, xx)))

    return dm[h-1][w-1]

def run():
    m = [ [ int(c) for c in l.strip() ] for l in sys.stdin ]
    print("Part 1:", solve(m))

    m2 = [ [ (c+i+j-1) % 9 + 1 for i in range(5) for c in r ] for j in range(5) for r in m ]
    print("Part 2:", solve(m2))

if __name__ == "__main__":
    run()
