from __future__ import division
from math import gcd


class Rational(object):
    def __init__(self, numer, denom):
        _gcd = gcd(numer, denom)
        sign = -1 if (numer < 0) ^ (denom < 0) else 1
        self.numer = sign * abs(numer) // _gcd
        self.denom = abs(denom) // _gcd

    def __eq__(self, other):
        return self.numer == other.numer and self.denom == other.denom

    def __repr__(self):
        return '{}/{}'.format(self.numer, self.denom)

    def __add__(self, other):
        return Rational(
            self.numer * other.denom + other.numer * self.denom,
            self.denom * other.denom
        )

    def __sub__(self, other):
        return Rational(
            self.numer * other.denom - other.numer * self.denom,
            self.denom * other.denom
        )

    def __mul__(self, other):
        return Rational(
            self.numer * other.numer,
            self.denom * other.denom
        )

    def __truediv__(self, other):
        return Rational(
            self.numer * other.denom,
            self.denom * other.numer
        )

    def __abs__(self):
        return Rational(
            abs(self.numer),
            abs(self.denom)
        )

    def __pow__(self, power):
        if isinstance(power, int):
            if power >= 0:
                return Rational(self.numer ** power, self.denom ** power)
            else:
                return Rational(self.denom ** abs(power), self.numer ** abs(power))
        elif isinstance(power, float):
            pow(self.numer / self.denom, power)
        return Rational(
            abs(self.numer ** power),
            abs(self.denom ** power)
        )

    def __rpow__(self, base):
        return pow(base, self.numer / self.denom)
