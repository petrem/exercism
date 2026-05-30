def tick(matrix: list[list[int]]) -> list[list[int]]:
    """Tick, tock, Conway's clock."""

    if (rows := len(matrix)) == 0:
        return []
    cols = len(matrix[0])

    def maybe_value(i, j):
        return matrix[i][j] if 0 <= i < rows and 0 <= j < cols else None

    def alive_neighbour_count(i, j):
        return sum(
            neighbour
            for di, dj in (
                (-1, -1),
                (-1, 0),
                (-1, 1),
                (0, -1),
                (0, 1),
                (1, -1),
                (1, 0),
                (1, 1),
            )
            if (neighbour := maybe_value(i + di, j + dj))
        )

    def conway_away(i, j, cell):
        alive_neighbours = alive_neighbour_count(i, j)
        print(f"{i}, {j} = {cell} -> {alive_neighbours}")
        if cell == 0 and alive_neighbours == 3:
            return 1
        if cell == 1 and alive_neighbours in (2, 3):
            return 1
        return 0

    return [
        [conway_away(i, j, cell) for j, cell in enumerate(row)]
        for i, row in enumerate(matrix)
    ]
