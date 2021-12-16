class Matrix(object):
    """The obvious approach using standard library only (so, no numpy).
    This is embelished to fill gaps with None.
    Thus, for example,
    Matrix("1 2 3\n4 5\n").column(2) == [3, None]
    """

    def __init__(self, matrix_string):
        self._matrix = [
            [int(e) for e in line.split()]
            for line in matrix_string.splitlines()
        ]
        self._nrows = len(self._matrix)
        self._ncols = max(len(self._matrix[i]) for i in range(self._nrows))

    def row(self, index):
        row = self._matrix[index - 1]
        return row + [None] * (self._ncols - len(row))

    def column(self, index):
        def _get(x, y):
            try:
                return self._matrix[x][y]
            except IndexError:
                return None
        return [_get(i, index - 1) for i in range(len(self._matrix))]
