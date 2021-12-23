"""Encode and decode with a rail-fence cipher."""
from itertools import accumulate, chain, cycle, islice
from typing import List


def encode(message: str, rails: int) -> str:
    """Encode `message` using `rails` fences."""
    return "".join(
        "".join(message[k]) for ks in _zigzag_indexes(len(message), rails) for k in ks
    )


def decode(encoded_message: str, rails: int) -> str:
    """Decode `encoded_message` that was encoded using `rails` fences."""
    zigzag = _zigzag_indexes(len(encoded_message), rails)
    row_starts = list(accumulate(map(len, zigzag), initial=0))
    row_iters = [iter(range(len(row))) for row in zigzag]
    down_and_up = cycle(chain(range(0, rails - 1), range(rails - 1, 0, -1)))
    return "".join(
        encoded_message[row_starts[j] + next(row_iters[j])]
        for j in islice(down_and_up, len(encoded_message))
    )


def _zigzag_indexes(maxlen: int, rails: int) -> List[List[int]]:
    return [
        # need list to be able to determine the length of each row
        list(_polirange(j, maxlen, (2 * (rails - j - 1), 2 * j)))
        for j in range(rails)
    ]


def _polirange(start, stop, steps):
    steps = cycle(filter(None, steps))
    while start < stop:
        yield start
        start += next(steps, None)
