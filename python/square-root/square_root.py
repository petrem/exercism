from collections import deque
from typing import Any, Iterable


def square_root(number: int) -> int:
    """Calculate integer square root."""
    return _last(_iter_approximate_sqrt(number))


def _iter_approximate_sqrt(radicand: int) -> Iterable:
    seed = (radicand.bit_length() + 1) // 2
    succ = seed
    prec = -1
    while prec != succ:
        prec = succ
        succ = (prec + radicand // prec) // 2  # next approximation
        yield succ


def _last(iterable: Iterable) -> Any:
    return deque(iterable, maxlen=1)[-1]
