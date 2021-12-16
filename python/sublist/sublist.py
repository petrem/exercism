"""
This exercise stub and the test suite contain several enumerated constants.

Since Python 2 does not have the enum module, the idiomatic way to write
enumerated constants has traditionally been a NAME assigned to an arbitrary,
but unique value. An integer is traditionally used because it’s memory
efficient.
It is a common practice to export both constants and functions that work with
those constants (ex. the constants in the os, subprocess and re modules).

You can learn more here: https://en.wikipedia.org/wiki/Enumerated_type
"""

from enum import Flag, auto


class ListRelation(Flag):
    UNEQUAL = 0
    SUBLIST = auto()
    SUPERLIST = auto()
    EQUAL = SUBLIST | SUPERLIST


# export module-level constants for compatiliblity with tests

UNEQUAL, SUBLIST, SUPERLIST, EQUAL = ListRelation


class SubList(list):

    def __contains__(self, other):
        if isinstance(other, list):
            if other == []:
                return True
            return any(self[i:i + len(other)] == other for i in self.indexes(other[0]))
        else:
            raise ValueError(f"Cannot check if contains a {type(other)}")

    def indexes(self, value):
        try:
            cursor = self.index(value)
            while cursor < len(self):
                yield cursor
                cursor = self.index(value, cursor + 1)
        except ValueError:
            pass


def sublist(list_one, list_two) -> ListRelation:
    l1 = SubList(list_one)
    l2 = SubList(list_two)

    relation = ListRelation.UNEQUAL
    if l1 in l2:
        relation |= ListRelation.SUBLIST
    if l2 in l1:
        relation |= ListRelation.SUPERLIST
    return relation


def sublist2(list_one, list_two) -> ListRelation:
    """A funnier (but slower!) way"""
    s1 = ",".join(str(x) for x in list_one)
    s2 = ",".join(str(x) for x in list_two)
    relation = ListRelation.UNEQUAL
    if s1 in s2:
        relation |= ListRelation.SUBLIST
    if s2 in s1:
        relation |= ListRelation.SUPERLIST
    return relation
