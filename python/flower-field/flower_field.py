from collections.abc import Iterable
from typing import Self


__all__ = ["annotate"]


class Matrix:
    def __init__(self, values: list[list[int]]):
        self._rows = len(values)
        self._cols = len(values[0])
        self._values = values

    @classmethod
    def zero(cls, rows: int, cols: int):
        return cls([[0] * cols for _ in range(rows)])

    def copy(self):
        return [list(xs) for xs in self._values]

    def __add__(self, other: Self) -> Self:
        return self.__class__(
            [
                [c1 + c2 for c1, c2 in zip(r1, r2, strict=True)]
                for r1, r2 in zip(self._values, other, strict=True)
            ]
        )

    def __setitem__(self, coord: tuple[int, int], value: int) -> None:
        self._values[coord[0]][coord[1]] = value

    def __str__(self):
        return "\n".join("".join(str(x) for x in row) for row in self._values) + "\n"

    def __iter__(self):
        return iter(self._values)

    def neighbours_of(self, coord: tuple[int, int]) -> Iterable[tuple[int, int]]:
        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                if (
                    not (dx == 0 and dy == 0)
                    and (0 <= (x := coord[0] + dx) < self._rows)
                    and (0 <= (y := coord[1] + dy) < self._cols)
                ):
                    yield x, y


def _generate_ones_around_each_flower(garden: list[str]) -> Iterable[Matrix]:
    rows = len(garden)
    cols = len(garden[0])
    for x, row in enumerate(garden):
        if len(row) != cols:
            raise ValueError("The board is invalid with current input.")
        for y, cell in enumerate(row):
            if cell == " ":
                continue
            if cell == "*":
                m = Matrix.zero(rows, cols)
                for coord in m.neighbours_of((x, y)):
                    m[coord] = 1
                yield m
            else:
                raise ValueError("The board is invalid with current input.")


def annotate(garden: list[str]) -> list[str]:
    if garden:
        rows = len(garden)
        cols = len(garden[0])
        return [
            "".join(
                "*" if orig_cell == "*" else " " if cell == 0 else str(cell)
                for orig_cell, cell in zip(orig_row, row, strict=True)
            )
            for orig_row, row in zip(
                garden,
                sum(
                    _generate_ones_around_each_flower(garden),
                    start=Matrix.zero(rows, cols),
                ),
                strict=True,
            )
        ]
    return []
