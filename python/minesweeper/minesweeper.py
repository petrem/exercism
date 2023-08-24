from itertools import product
from typing import Iterable


Minefield = list[str]


def annotate(minefield: Minefield) -> Minefield:
    board = Board(minefield)
    board.annotate()
    return board.to_minefield()


class Board:
    DIRECTIONS = [(dx, dy) for (dx, dy) in product([-1, 0, 1], [-1, 0, 1]) if dx or dy]

    def __init__(self, minefield: Minefield) -> None:
        self._validate_minefield(minefield)
        self.board = [list(row) for row in minefield]
        self.n_rows = len(minefield)
        self.n_cols = len(minefield[0]) if self.n_rows else 0

    def annotate(self) -> None:
        for x in range(self.n_rows):
            for y in range(self.n_cols):
                if not self.is_mine(x, y):
                    mines = self.count_neighbor_mines(x, y)
                    self.board[x][y] = str(mines) if mines else " "

    def _validate_minefield(self, minefield: Minefield) -> None:
        if not isinstance(minefield, list):
            raise ValueError("The minefield is not a list.")
        if not all(isinstance(s, str) for s in minefield):
            raise ValueError("The minefield contains non-str rows.")
        if any(len(s) != len(minefield[0]) for s in minefield):
            raise ValueError("The board is invalid with current input.")
        if any(set(s) - set(" *") for s in minefield):
            raise ValueError("The board is invalid with current input.")

    def is_on_board(self, x: int, y: int) -> bool:
        return 0 <= x < self.n_rows and 0 <= y < self.n_cols

    def is_mine(self, x: int, y: int) -> bool:
        return self.board[x][y] == "*"

    def cell_neighbors(self, x: int, y: int) -> Iterable[tuple[int, int]]:
        for dx, dy in self.DIRECTIONS:
            coord = x + dx, y + dy
            if self.is_on_board(*coord):
                yield coord

    def count_neighbor_mines(self, x: int, y: int) -> int:
        return sum(self.is_mine(i, j) for i, j in self.cell_neighbors(x, y))

    def to_minefield(self) -> Minefield:
        return [''.join(row) for row in self.board]


