from __future__ import annotations

import re
from itertools import product
from math import ceil, sqrt
from typing import Tuple


def cipher_text(plain_text: str) -> str:
    normalized = re.sub(r"\W", "", plain_text).lower()
    if normalized:
        return _square_crypt(normalized)
    return ""


def _square_crypt(message: str) -> str:
    c, r = _get_cols_rows(len(message))
    padding = " " * (c * r - len(message))
    message += padding
    # # Previous:
    # return " ".join(
    #     "".join(message[i * c + j] for i in range(r))
    #     for j in range(c)
    # )
    # This was was stolen from https://exercism.io/tracks/python/exercises/crypto-square/solutions/024dc3efe767411f86af7740e6358434
    return " ".join(message[i::c] for i in range(c))


# This is intuitive and seems to work
def _get_cols_rows(n: int) -> Tuple[int, int]:
    cols = ceil(sqrt(n))
    rows = ceil(n / cols)
    return cols, rows


# Alternatively, this searches for a solution
# Yields the same results for integers upto 10000 at least...
# TODO: I should think about the math
def _get_cols_rows2(n: int) -> Tuple[int, int]:
    return min(
        (c, r)
        for c, r in product(_range_downfrom_sqrt(n), _range_downfrom_sqrt(n))
        if c >= r and c - r <= 1 and c * r >= n
    )


def _range_downfrom_sqrt(n: int) -> range:
    return range(ceil(sqrt(n)), 0, -1)
