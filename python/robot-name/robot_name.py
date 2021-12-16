#!/usr/bin/env python3

from functools import reduce
from itertools import accumulate, islice
from operator import mul
import random
import string


NAME_FORMAT = "LLDDD"  # L - letter, D - digit
FORMAT_MAP = {
    "L": string.ascii_uppercase,
    "D": string.digits,
}


def divmod(dividend, divisor):
    """Returns both the quotient and the remainder as tuple. Similar to numpy.divmod."""
    return dividend // divisor, dividend % divisor


def dropfirst(iterable):
    """Drop the first element of an iterable."""
    return islice(iterable, 1, None)


def remainders(dividend, divisors):
    """Repeatedly divide to the divisors, starting with dividend, and return
    the list of remainders."""
    return map(  # extract remainder, the second element of the accumulated tuples
        lambda t: t[1],
        dropfirst(  # drop list head, it does not contain a remainder
            accumulate(
                ((dividend, -1), *divisors),
                lambda acc, x: divmod(acc[0], x)
            )
        )
    )


def _name_from_random_value(name_format, rnd):
    elements = [len(FORMAT_MAP[x]) for x in name_format]
    return "".join(
        [FORMAT_MAP[x][r] for x, r in zip(name_format, remainders(rnd, elements))])


def generate_names(name_format):
    """Generate names in the required format."""
    name_space_size = reduce(mul, (len(FORMAT_MAP[x]) for x in name_format), 1)
    while True:
        randomness = random.randint(0, name_space_size)
        yield _name_from_random_value(name_format, randomness)


class Robot(object):
    USED_NAMES = set()

    def __init__(self, name_format=NAME_FORMAT):
        self._name_format = name_format
        self.reset()

    @property
    def name(self):
        return self._name

    def _generate_name(self):
        generated = next(
            g for g in generate_names(self._name_format) if g not in self.USED_NAMES)
        self.USED_NAMES.add(generated)
        return generated

    def reset(self):
        self._name = self._generate_name()
