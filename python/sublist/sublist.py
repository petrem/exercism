from enum import Flag, auto, global_enum


@global_enum
class ListRelation(Flag):
    UNEQUAL = 0
    SUBLIST = auto()
    SUPERLIST = auto()
    EQUAL = SUBLIST | SUPERLIST


class SubList(list):
    def __contains__(self, other):
        if isinstance(other, list):
            if other == []:
                return True
            return any(
                self[i : i + len(other)] == other for i in self.indexes(other[0])
            )
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

    relation = UNEQUAL
    if l1 in l2:
        relation |= SUBLIST
    if l2 in l1:
        relation |= SUPERLIST
    return relation
