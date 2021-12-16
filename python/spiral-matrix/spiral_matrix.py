from __future__ import annotations

import typing as T
from itertools import chain, repeat
from dataclasses import dataclass


def spiral_matrix(size):
    if size == 0:
        return []
    matrix = [[None] * size for _ in range(size)]
    cursor = XY(0, -1)
    for value, direction in enumerate(_walk_spiral(size), start=1):
        cursor += direction
        matrix[cursor.x][cursor.y] = value
    return matrix


@dataclass
class XY():
    x: int
    y: int

    def __add__(self: XY, other: XY) -> XY:
        return XY(self.x + other.x, self.y + other.y)


RIGHT = XY(0, 1)
DOWN = XY(1, 0)
LEFT = XY(0, -1)
UP = XY(-1, 0)


def _walk_spiral(n: int) -> T.Iterable[XY]:
    k = n - 1
    while k > 0:
        yield from chain(
            repeat(RIGHT, k + 1), repeat(DOWN, k), repeat(LEFT, k), repeat(UP, k - 1)
        )
        k = k - 2
    if n % 2 == 1:
        yield RIGHT
