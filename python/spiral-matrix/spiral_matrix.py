from itertools import chain, repeat


def spiral_matrix(size):
    matrix = [[None] * size for _ in range(size)]
    x, y = 0, -1
    for value, direction in enumerate(_walk_spiral(size), start=1):
        x, y = x + direction[0], y + direction[1]
        matrix[x][y] = value
    return matrix


RIGHT, DOWN, LEFT, UP = (0, 1), (1, 0), (0, -1), (-1, 0)


def _walk_spiral(n):
    for k in range(n - 1, 0, -2):
        yield from chain(
            repeat(RIGHT, k + 1), repeat(DOWN, k), repeat(LEFT, k), repeat(UP, k - 1)
        )
    if n % 2 == 1:
        yield RIGHT
