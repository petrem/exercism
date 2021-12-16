from typing import List


def convert(input_grid: List[str]) -> str:
    n_rows = len(input_grid)
    n_cols = len(input_grid[0])

    if n_rows % 4 != 0 or n_cols % 3 != 0:
        raise ValueError("Input grid must be a multiple of 3x4")
    if any(len(input_grid[i]) != n_cols for i in range(1, n_rows)):
        raise ValueError("Found rows with different number of columns")

    cell_rows = (input_grid[r:r + 4] for r in range(0, n_rows, 4))

    def get_cells(cell_row):
        return [
            [cell_row[r][c:c + 3] for r in range(4)]
            for c in range(0, n_cols, 3)
        ]

    return ",".join(
        "".join(recognize(cell) for cell in get_cells(cell_row))
        for cell_row in cell_rows
    )


PATTERNS = [
    "   ",  # 0
    " _ ",  # 1
    "  |",  # 2
    " _|",  # 3
    "|_|",  # 4
    "|_ ",  # 5
    "| |",  # 6
]


DIGIT_PATTERNS = {
    (1, 6, 4): 0,
    (0, 2, 2): 1,
    (1, 3, 5): 2,
    (1, 3, 3): 3,
    (0, 4, 2): 4,
    (1, 5, 3): 5,
    (1, 5, 4): 6,
    (1, 2, 2): 7,
    (1, 4, 4): 8,
    (1, 4, 3): 9,
}


def recognize(cell: List[str]) -> str:
    try:
        return str(
            DIGIT_PATTERNS[tuple(PATTERNS.index(cell[row]) for row in range(3))]
        )
    except (KeyError, ValueError):
        return "?"
