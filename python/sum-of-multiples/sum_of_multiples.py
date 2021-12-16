from functools import partial
from math import ceil
from operator import ne


def sum_of_multiples(limit, multiples):
    return sum(
        set(
            [
                m * i for m in filter(partial(ne, 0), multiples)
                for i in range(1, ceil(limit / m))
            ]
        )
    )
