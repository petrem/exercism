from functools import reduce
from operator import or_


def square(number) -> int:
    _check_square(number)
    return 1 << (number - 1)


def total(number=64) -> int:
    _check_square(number)
    # I know this is not necessary, I know there's a formula, etc.
    return reduce(or_, (square(i) for i in range(1, number + 1)))


def _check_square(number):
    if not (isinstance(number, int) and 0 < number <= 64):
        raise ValueError("square must be between 1 and 64")
