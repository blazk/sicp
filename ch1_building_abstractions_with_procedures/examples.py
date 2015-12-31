#!/usr/bin/env python


# 1.1.7: Square Root by Newton's Method

def average(a, b):
    return (a + b) / 2.0

def square(x):
    return x * x

def sqrt(x):
    def is_good_enough(guess):
        return abs(square(guess) - x) < 0.001
    def improve(guess):
        return average(guess, x/guess)
    def iter(guess):
        if is_good_enough(guess):
            return guess
        return iter(improve(guess))
    return iter(1.0)

print "1.1.7 sqrt"
print sqrt(9.0)
