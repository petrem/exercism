from functools import wraps


class Counting:
    def __init__(self):
        self._comparisons = 0
        self._assignments = 0

    def __str__(self):
        return f"Counting: {self._comparisons} comparisons, {self._assignments} assignments"
    @property
    def operations(self):
        return self._comparisons + self._assignments

    @staticmethod
    def comparision(fn):
        @wraps(fn)
        def counting(self, *args):
            self._comparisons += 1
            return fn(self, *args)
        return counting

    @staticmethod
    def assignment(fn):
        @wraps(fn)
        def counting(self, *args):
            self._assignments += 1
            return fn(self, *args)
        return counting

    @comparision
    def lt(self, a, b):
        return a < b

    @comparision
    def le(self, a, b):
        return a <= b

    @assignment
    def list_assign(self, sequence, index, element):
        sequence[index] = element

    @assignment
    def list_element_copy(self, sequence, index1, index2):
        sequence[index1] = sequence[index2]

    @assignment
    def list_swap(self, sequence, index1, index2):
        tmp = sequence[index1]
        sequence[index1] = sequence[index2]
        sequence[index2] = tmp


MIN = 0
MID = 1
MAX = 2


def insert_top3_v1(sequence, k):
    """Insert `k` into sorted `sequence` discarding least value."""
    c = Counting()
    # assume sequence is [3, 5, 7] and k <- [2..8]
    if c.le(k, sequence[MID]):  # 2,3,4,5
        if c.le(k, sequence[MIN]):  # 2,3
            pass
        else:  # 4, 5
            c.list_assign(sequence, MIN, k)
    else:  # 6,7,8
        c.list_element_copy(sequence, MIN, MID)
        if c.lt(k, sequence[MAX]):  # 6, 7
            c.list_assign(sequence, MID, k)
        else:  # 8
            c.list_element_copy(sequence, MID, MAX)
            c.list_assign(sequence, MAX, k)
    return c
