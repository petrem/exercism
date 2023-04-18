"""An obviously erroneous solution that passes the tests.

Evidently, the hash table implementation does not handle collisions.
"""

from itertools import chain


class CustomSet:
    _HASHTABLE_SIZE = 7

    def __init__(self, elements=[]):
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
        return len(self) == len(other) and all(
            e in self for e in other
        )

    def __iter__(self):
        return iter(self._hashtable)

    def add(self, element):
        self._hashtable.add(element)

    def intersection(self, other):
        return CustomSet(
            e for e in other if e in self
        )

    def __sub__(self, other):
        return CustomSet(e for e in self if e not in other)

    def __add__(self, other):
        return CustomSet(chain(self, other))

    def __len__(self):
        return len(self._hashtable)

    def __repr__(self):
        elements = ",".join(self._hashtable)
        return f"CustomSet([{elements}])"


class _HashTable:
    NIL = object()

    def __init__(self, size: int):
        self.size = size
        self._table = [_HashTable.NIL] * size

    def _hash(self, obj):
        return id(obj) % self.size

    def __len__(self):
        return sum(1 for e in self._table if e is not _HashTable.NIL)

    def __contains__(self, element):
        return self._table[self._hash(element)] is not _HashTable.NIL

    def __iter__(self):
        return _HashTableIterator(self._table)

    def add(self, element):
        self._table[self._hash(element)] = element

    def __repr__(self):
        elements = ",".join(str(e) for e in self._table if e is not _HashTable.NIL)
        return f"_HashTable([{elements}])"


class _HashTableIterator:
    def __init__(self, table):
        self._table = table
        self._current = -1

    def __next__(self):
        self._current += 1
        while self._current < len(self._table):
            element = self._table[self._current]
            if element is not _HashTable.NIL:
                return element
            self._current += 1
        raise StopIteration

    def __iter__(self):
        return self
