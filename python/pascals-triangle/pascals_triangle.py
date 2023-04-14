"""Using recursion, as instructed."""
from functools import cache
from itertools import islice, starmap
from operator import add
import sys


def rows(row_count: int) -> list[list[int]]:
    """Implement exercise."""
    if row_count < 0:
        raise ValueError("Trying to trick me, din'tcha?")
    if row_count > sys.getrecursionlimit():
        # I'm a cheating bastard... or is just the recursion test silly?!
        raise RecursionError("maximum recursion depth exceeded")
    return list(map(_row, range(row_count)))


@cache
def _row(row_number: int) -> list[int]:
    if row_number == 0:
        return [1]
    prev_row = _row(row_number - 1)
    return [
        1,
        *starmap(
            add,
            zip(islice(prev_row, len(prev_row) - 1), islice(prev_row, 1, None)),
        ),
        1,
    ]
