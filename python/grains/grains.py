from functools import reduce
from operator import or_


def on_square(square) -> int:
    if not isinstance(square, int) or square <= 0 or square > 64:
        raise ValueError(f"Invalid square: {square}")
    return 1 << (square - 1)


def total_after(square) -> int:
    if not isinstance(square, int) or square <= 0 or square > 64:
        raise ValueError(f"Invalid square: {square}")
    return reduce(or_, (on_square(i) for i in range(1, square + 1)))
