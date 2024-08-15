"""Raindrops (ursoidea version)."""
from collections.abc import Iterable
from functools import singledispatchmethod
from itertools import compress
from typing import Self


class NamedInt(int):
    """Ints with names.

    This is a gimmick, among others because operations yield simple ints.
    """

    def __new__(cls, name, *args):
        value = super().__new__(cls, *args)
        value.name = name
        return value

    def __str__(self):
        return self.name  # pylint: disable=no-member


class Vector(list):
    """Simpleton relative of some Pandas and other bears."""

    @singledispatchmethod
    def __getitem__(self, index):
        return super().__getitem__(index)

    @__getitem__.register
    def _(self, selectors: Iterable) -> Self:
        return self.__class__(compress(self, selectors))

    def __rmod__(self, other):
        return self.__class__(other % x for x in self)

    @singledispatchmethod
    def __eq__(self, other):
        return self.__class__(x == other for x in self)

    @__eq__.register
    def _(self, other: Iterable):
        return super().__eq__(other)

    def __str__(self):
        return "".join(map(str, self))


RAINSPEAK = Vector([NamedInt("Pling", 3), NamedInt("Plang", 5), NamedInt("Plong", 7)])


def convert(number):
    """Convert `number` to raindrop-speak."""

    return str(RAINSPEAK[number % RAINSPEAK == 0] or number)
