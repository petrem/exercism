"""Exercise: sum-of-multiples"""
from functools import partial
from math import ceil
from operator import ne


def sum_of_multiples(limit, multiples):
    """Calculate sum of multiples less than ``limit``."""
    return sum(
        {
            m * i
            for m in filter(partial(ne, 0), multiples)
            for i in range(1, ceil(limit / m))
        }
    )
