from typing import List, Tuple

Minefield = List[str]


def annotate(minefield: Minefield) -> Minefield:
    # Function body starts here
    board = Board(minefield)
    board.annotate()
    return board.to_minefield()


class Board:

    def __init__(self, minefield: Minefield) -> None:
        self._validate_minefield(minefield)
        self.board = [[c for c in row] for row in minefield]
        self.n_rows = len(minefield)
        self.n_cols = len(minefield[0]) if self.n_rows else 0

    def annotate(self):
        for x in range(self.n_rows):
            for y in range(self.n_cols):
                if not self.is_mine(x, y):
                    mines = self.count_neighbor_mines(x, y)
                    self.board[x][y] = str(mines) if mines else " "

    def _validate_minefield(self, minefield: Minefield) -> None:
        if not isinstance(minefield, list):
            raise ValueError("The minefield is not a list")
        if not all(isinstance(s, str) for s in minefield):
            raise ValueError("The minefield contains non-str rows")
        if not all(len(s) == len(minefield[0]) for s in minefield):
            raise ValueError("The minefield contains rows of different length")
        if any(set(s) - set(" *") for s in minefield):
            raise ValueError("The minefield contains invalid characters")

    def is_on_board(self, x: int, y: int) -> bool:
        return 0 <= x < self.n_rows and 0 <= y < self.n_cols

    def is_mine(self, x: int, y: int) -> bool:
        return self.board[x][y] == "*"

    def cell_neighbors(self, x: int, y: int) -> List[Tuple[int, int]]:
        return [
            (x + delta_x, y + delta_y)
            for delta_x in range(-1, 2)
            for delta_y in range(-1, 2)
            if (
                self.is_on_board(x + delta_x, y + delta_y)
                and (delta_x != 0 or delta_y != 0)
            )
        ]

    def count_neighbor_mines(self, x: int, y: int) -> int:
        return sum(self.is_mine(i, j) for (i, j) in self.cell_neighbors(x, y))

    def to_minefield(self) -> Minefield:
        return [''.join(row) for row in self.board]
