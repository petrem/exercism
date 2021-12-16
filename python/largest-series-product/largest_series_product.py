from collections import deque
from functools import reduce
from itertools import islice
from operator import mul


def largest_product(series, size):
    series_iterator = iter(int(d) for d in series)
    window = deque(islice(series_iterator, size), size)
    if len(window) < size:
        raise ValueError("Input series shorter than window size")
    maximum = product(window)
    for digit in series_iterator:
        window.append(digit)
        maximum = max(maximum, product(window))
    return maximum


def product(iterable):
    return reduce(mul, list(iterable), 1)
