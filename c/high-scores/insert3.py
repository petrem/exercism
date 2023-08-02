from functools import wraps


class Counting:
    def __init__(self, comparisons=0, assignments=0):
        self._comparisons = comparisons
        self._assignments = assignments

    def __str__(self):
        return f"Counting: {self._comparisons} comparisons, {self._assignments} assignments"

    def __add__(self, other):
        assert isinstance(other, Counting)
        return Counting(
            self._comparisons + other._comparisons,
            self._assignments + other._assignments,
        )

    @property
    def operations(self):
        return self._comparisons + self._assignments

    @staticmethod
    def comparison(fn):
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

    @comparison
    def lt(self, a, b):
        return a < b

    @comparison
    def le(self, a, b):
        return a <= b

    @comparison
    def ge(self, a, b):
        return a >= b

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
    """Insert `k` into sorted `sequence` discarding least value.

    Start in the middle, break down cases in approximate halves.

    Results in pretty evenly spread number of operations depending on position of
    insertion. This is not necessarily good.

    """
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


def insert_top3_v2(sequence, k):
    """Insert `k` into sorted `sequence` discarding least value.

    Get rid of extremes first, starting with values that don't make the top.
    """
    c = Counting()
    # assume sequence is [3, 5, 7] and k <- [2..8]
    if c.le(k, sequence[MIN]):  # 2,3
        pass
    elif c.ge(k, sequence[MAX]):  # 7, 8
        c.list_element_copy(sequence, MIN, MID)
        c.list_element_copy(sequence, MID, MAX)
        c.list_assign(sequence, MAX, k)
    elif c.le(k, sequence[MID]):  # 4, 5
        c.list_assign(sequence, MIN, k)
    else:  # 6
        c.list_element_copy(sequence, MIN, MID)
        c.list_assign(sequence, MID, k)
    return c


def insert_top3_v3(sequence, k):
    """Insert `k` into sorted `sequence` discarding least value."""
    c = Counting()
    # assume sequence is [3, 5, 7] and k <- [2..8]
    if c.le(k, sequence[MIN]):  # 2,3
        pass
    elif c.le(k, sequence[MID]):  # 4, 5
        c.list_assign(sequence, MIN, k)
    elif c.lt(k, sequence[MAX]):  # 6
        c.list_element_copy(sequence, MIN, MID)
        c.list_assign(sequence, MID, k)
    else:  # 7, 8
        c.list_element_copy(sequence, MIN, MID)
        c.list_element_copy(sequence, MID, MAX)
        c.list_assign(sequence, MAX, k)
    return c


def get_top3(scores, top3_fn):
    top3 = sorted(scores[:3])
    c = Counting()
    for k in scores[3:]:
        c += top3_fn(top3, k)
    return top3, c
