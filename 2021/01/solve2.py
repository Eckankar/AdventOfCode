#!/usr/bin/env python3
import sys

nums = [ int(num) for num in sys.stdin ]
windowed = [ a+b+c for (a, b, c) in zip(nums, nums[1:], nums[2:]) ]
increases = [ 1 for (a, b) in zip(windowed, windowed[1:]) if a < b ]

print(len(increases))
