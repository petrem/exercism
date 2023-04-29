"""Custom Sets: or is unit testing pointless?

(No, it is not, but we have to be careful)


Iternation 1: An erroneous solution that passes the tests. The most evident issue is
that the hash table implementation does not handle collisions.

A small number of tests re-written using `hypothesis` exposes the problem quickly: given
we use 7 as the hashtable size and as the prime for the hash function (division-based):

```
FAILED custom_set_prop_test.py::test_when_the_element_is_not_in_the_set - assert (7 not in CustomSet([0])) is True
```

See `custom_set_prop_test.py`. Assumes you have `pytest` and `hypothesis` installed.

Iteration 2: Adding chaining solves the exposed issues. Changed the hash function to a
multiplicaton-based one and use `hash()` instead of `id()` which of course caused issues
for equal, but distinct objects. There are other various minor updates, and more
hypothesis-based testing. If you suspect I'm fairly new at using `hypothesis`, you are
correct.

"""
import math
import sys
from copy import deepcopy


class CustomSet:
    _HASHTABLE_SIZE = 7

    def __init__(self, elements=()):
        self._hashtable = _HashTable(CustomSet._HASHTABLE_SIZE)
        for element in elements:
            self.add(element)

    def isempty(self):
        return len(self._hashtable) == 0

    def __contains__(self, element):
        return element in self._hashtable

    def issubset(self, other):
        return all(e in other for e in self)

    def isdisjoint(self, other):
        return self.intersection(other).isempty()

    def __eq__(self, other):
        return len(self) == len(other) and all(e in self for e in other)

    def add(self, element):
        self._hashtable.add(element)

    def intersection(self, other):
        return CustomSet(e for e in other if e in self)

    def __sub__(self, other):
        return CustomSet(e for e in self if e not in other)

    def __add__(self, other):
        larger, smaller = (self, other) if len(self) >= len(other) else (other, self)
        union = larger.copy()
        for element in smaller:
            union.add(element)
        return union

    # Extra-curricular methods -- consider them private (e.g. for tests purposes).

    def copy(self):
        obj = super().__new__(CustomSet)
        obj._hashtable = self._hashtable.copy()
        return obj

    def __iter__(self):
        return iter(self._hashtable)

    def __len__(self):
        return len(self._hashtable)

    def __repr__(self):
        elements = ",".join(repr(e) for e in self._hashtable)
        return f"CustomSet([{elements}])"


WORD_WIDTH = sys.hash_info.width
NEXT_POWER_OF_TWO = 2 ** (WORD_WIDTH + 1)


class _HashTable:
    s_Factor = int(2 ** (WORD_WIDTH - 1) * (math.sqrt(5) - 1))

    def __init__(self, size_exp: int):
        if not 0 < size_exp <= WORD_WIDTH:
            raise ValueError(
                f"Hash size exponent {size_exp} must be between 1 and {WORD_WIDTH}"
            )
        self.hash_bits_displacement = WORD_WIDTH - size_exp
        self.hash_mask = (2**size_exp - 1) << self.hash_bits_displacement
        self.size = 2**size_exp
        self.len = 0
        self._table = [[] for _ in range(self.size)]

    def _hash(self, obj):
        k = hash(obj)
        if k < 0:
            k = NEXT_POWER_OF_TWO - k
        return (
            (k * _HashTable.s_Factor) & self.hash_mask
        ) >> self.hash_bits_displacement

    def __len__(self):
        return self.len

    def __contains__(self, element):
        return element in self._table[self._hash(element)]

    def __iter__(self):
        return _HashTableIterator(self._table)

    def add(self, element):
        hv = self._hash(element)
        if element not in self._table[hv]:
            self._table[hv].append(element)
            self.len += 1

    def extend(self, elements):
        """Add several elements at once.

        Maybe we could make it more efficient then adding them one by one?.
        """
        for element in elements:
            self.add(element)

    def copy(self):
        clone = super().__new__(_HashTable)
        clone.size = self.size
        clone.len = self.len
        clone.hash_mask = self.hash_mask
        clone.hash_bits_displacement = self.hash_bits_displacement
        clone._table = deepcopy(self._table)
        return clone

    def __repr__(self):
        elements = ",".join(repr(e) for e in self)
        return f"_HashTable([{elements}])"


class _HashTableIterator:
    def __init__(self, table):
        self._table = table
        self._current_slot = 0
        self._current_in_chain = 0

    def __next__(self):
        while self._current_slot < len(self._table):
            slot_chain = self._table[self._current_slot]
            if self._current_in_chain < len(slot_chain):
                element = slot_chain[self._current_in_chain]
                self._current_in_chain += 1
                return element
            self._current_in_chain = 0
            self._current_slot += 1
        raise StopIteration

    def __iter__(self):
        return self
