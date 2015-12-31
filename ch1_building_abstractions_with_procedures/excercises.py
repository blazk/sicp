#!/usr/bin/env python


# Excercise 1.4: Compound operators

import operator

def a_plus_abs_b(a, b):
    f = b < 0 and operator.sub or operator.add
    return f(a, b)

print '1.4 a_plus_abs_b'
print a_plus_abs_b(2,-2)    #prints '4'
print a_plus_abs_b(2, 2)    #prints '4'
