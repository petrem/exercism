"""Exercise: largest-series-product"""
from collections import deque
from functools import reduce
from itertools import islice
from operator import mul


def largest_product(series, size):
    """Find largest series product with specified size."""
    if size < 0:
        raise ValueError("span must not be negative")
    series_iterator = iter(int(d) for d in series)
    window = deque(islice(series_iterator, size), size)
    if len(window) < size:
        raise ValueError("span must be smaller than string length")
    maximum = _product(window)
    try:
        for digit in series_iterator:
            window.append(digit)
            maximum = max(maximum, _product(window))
    except ValueError as e:
        raise ValueError("digits input must only contain digits") from e
    return maximum


def _product(iterable):
    return reduce(mul, list(iterable), 1)
