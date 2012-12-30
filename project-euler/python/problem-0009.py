#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math

# http://projecteuler.net/problem=9
# 
# A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
# 
# a^2 + b^2 = c^2
# 
# For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
# 
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
#
# Find the product abc.

def problem_0009():
    '''Solve the above problem.'''

    a, b, c = pythagorean_sum_equals(1000)
    return a * b * c

def problem_0009_nogen():
    '''Solve the above problem.'''

    a, b, c = pythagorean_sum_equals_nogen(1000)
    return a * b * c

def pythagorean_triplets():
    '''Generate all pythagorean triples with a < b, ordered by b.'''

    b = 0
    while True:
        b += 1
        for a in xrange(1, b + 1):
            c = math.sqrt(a * a + b * b) 
            if c == int(c):
                yield a, b, int(c)

def pythagorean_sum_equals(n):
    '''Find all pythagorean triples where a+b+c = n.'''

    for a, b, c in pythagorean_triplets():
        if a + b + c == n:
            return a, b, c

def pythagorean_sum_equals_nogen(n):
    '''Find all pythagorean triples where a+b+c = n.'''

    b = 0
    while True:
        b += 1
        for a in xrange(1, b + 1):
            c = math.sqrt(a * a + b * b) 
            if c == int(c) and a + b + c == n:
                return a, b, c
import time
s = time.time()
r = problem_0009()
print '%0.3f seconds, answer = %s\n' % (time.time() - s, r)