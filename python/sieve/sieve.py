from array import array
from itertools import repeat
from math import sqrt, trunc


def primes(limit):
    marks = BitArray(limit)
    mark_until = trunc(sqrt(limit))
    for i in range(2, mark_until + 1):
        if marks[i] == 0:
            for j in range(i * i, limit + 1, i):
                marks[j] = 1

    marks[0] = marks[1] = 1  # skip 0 and 1 which are not primes
    return [candidate for candidate, marked in enumerate(marks) if not marked]


class BitArray():
    WORDTYPE = "L"
    WORDSIZE = 8 * 8
    WORDMASK = WORDSIZE - 1

    def __init__(self, size):
        self._size = size
        self._words = array(
            BitArray.WORDTYPE,
            repeat(0, (size + BitArray.WORDSIZE - 1) // BitArray.WORDSIZE)
        )

    def __getitem__(self, index):
        word = self._words[index // BitArray.WORDSIZE]
        return (word >> (index & BitArray.WORDMASK)) & 1

    def __setitem__(self, index, value):
        word_index = index // BitArray.WORDSIZE
        shift = index & BitArray.WORDMASK
        word = self._words[word_index]
        if (word >> shift) & 1 != value:
            self._words[word_index] = word ^ (1 << shift)

    def __str__(self):
        return "[" + ", ".join(str(self[i]) for i in range(0, self._size + 1)) + "]"

    def __iter__(self):
        return self._iterate()

    def _iterate(self):
        for i in range(0, self._size + 1):
            yield self[i]
