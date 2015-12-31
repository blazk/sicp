#!/usr/bin/env python

"""
Example 2.3.2: Symbolic differentiation
"""


def deriv(exp, var):
    if is_const(exp, var):
        return 0
    if is_var(exp, var):
        return 1
    if isinstance(exp, Sum):
        return Sum(
                deriv(exp.left, var),
                deriv(exp.right, var))
    if isinstance(exp, Prod):
        return Sum(
                Prod(
                    exp.left,
                    deriv(exp.right, var)),
                Prod(
                    exp.right,
                    deriv(exp.left, var)))
    raise TypeError(
            'unexpected expr type ({})'.format(
            type(exp).__name__))


#
# representation of expression objects
#

def is_atom(exp):
    return not isinstance(exp, BinOp)
def is_num(exp):
    return is_atom(exp) and isinstance(exp, int)
def is_const(exp, var):
    return is_atom(exp) and exp != var
def is_var(exp, var):
    return is_atom(exp) and exp == var

class BinOp(object):
    def __init__(self, left, right):
        self.left = left
        self.right = right

class Sum(BinOp):
    op = ' + '
    rank = 0
    def __new__(cls, left, right):
        # simplification applied at construction time
        if is_num(left) and is_num(right):
            return left + right
        if left == 0:
            return right
        if right == 0:
            return left
        return object.__new__(cls, left, right)

class Prod(BinOp):
    op = '*'
    rank = 1
    def __new__(cls, left, right):
        # simplification applied at construction time
        if left == 0 or right == 0:
            return 0
        if right == 1:
            return left
        if left == 1:
            return right
        if is_num(left) and is_num(right):
            return left * right
        return object.__new__(cls, left, right)


#
# rendering expression tree as string
#

class ExpRenderer(object):
    def __call__(self, exp):
        s, _ = self.render(exp)
        return s
    def render(self, exp):
        handler = 'render_' + type(exp).__name__
        return getattr(self, handler, None)(exp)
    def render_str(self, exp):
        return exp, 99
    def render_int(self, exp):
        return str(exp), 99
    def render_bin_op(self, exp):
        lstr, lrank = self.render(exp.left)
        rstr, rrank = self.render(exp.right)
        if lrank < exp.rank:
            lstr = '(' + lstr + ')'
        if rrank < exp.rank:
            rstr = '(' + rstr + ')'
        return lstr + exp.op + rstr, exp.rank
    def render_Sum(self, exp):
        return self.render_bin_op(exp)
    def render_Prod(self, exp):
        return self.render_bin_op(exp)



if __name__ == '__main__':

    e2s = ExpRenderer()

    # an example: derivative of "a*x*x + b*x + c"

    exp = Sum(Prod('a', Prod('x', 'x')),
            Sum(Prod('b', 'x'),
                'c'))

    dd = deriv(exp, 'x')
    print e2s(dd)           # prints "a*(x + x) + b"

    #dd = deriv(exp, 'a')
    #print e2s(dd)           # FAILS!, infinite recursion, why??

    dd = deriv(exp, 'b')
    print e2s(dd)           # prints "x"

    dd = deriv(exp, 'c')
    print e2s(dd)           # prints "1"
