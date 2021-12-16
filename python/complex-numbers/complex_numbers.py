from math import cos, exp, sin, sqrt
import weakref


class ComplexNumber(object):

    COMPLEX_NUMBERS = weakref.WeakValueDictionary()

    def __new__(cls, real, imaginary):
        for number, instance in cls.COMPLEX_NUMBERS.items():
            if number[0] == real and number[1] == imaginary:
                return instance
        new_instance = super().__new__(cls)
        cls.COMPLEX_NUMBERS[(real, imaginary)] = new_instance
        return new_instance

    def __init__(self, real, imaginary):
        self.real = real
        self.imaginary = imaginary

    def __eq__(self, other):
        return self.real == other.real and self.imaginary == other.imaginary

    def __ne__(self, other):
        return self.real != other.real or self.imaginary != other.imaginary

    def __add__(self, other):
        if isinstance(other, ComplexNumber):
            return ComplexNumber(
                self.real + other.real,
                self.imaginary + other.imaginary
            )
        elif isinstance(other, (int, float)):
            return ComplexNumber(self.real + other, self.imaginary)
        else:
            raise TypeError("Cannot add with %s" % type(other))

    __radd__ = __add__

    def __mul__(self, other):
        if isinstance(other, ComplexNumber):
            return ComplexNumber(
                self.real * other.real - self.imaginary * other.imaginary,
                self.imaginary * other.real + self.real * other.imaginary,
            )
        elif isinstance(other, (int, float)):
            return ComplexNumber(self.real * other, self.imaginary * other)
        else:
            raise TypeError("Cannot multiply with %s" % type(other))

    __rmul__ = __mul__

    def __sub__(self, other):
        return ComplexNumber(self.real - other.real, self.imaginary - other.imaginary)

    def __truediv__(self, other):
        return ComplexNumber(
            (self.real * other.real + self.imaginary * other.imaginary)
            / (other.real ** 2 + other.imaginary ** 2),
            (self.imaginary * other.real - self.real * other.imaginary)
            / (other.real ** 2 + other.imaginary ** 2),
        )

    def __abs__(self):
        return sqrt(self.real ** 2 + self.imaginary ** 2)

    def __str__(self):
        return f"{self.real} + {self.imaginary}j"

    def __repr__(self):
        return f"ComplexNumber({self.real}, {self.imaginary})"

    def conjugate(self):
        return ComplexNumber(self.real, -self.imaginary)

    def exp(self):
        return exp(self.real) * ComplexNumber(cos(self.imaginary), sin(self.imaginary))
