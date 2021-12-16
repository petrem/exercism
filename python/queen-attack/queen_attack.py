class Queen:
    def __init__(self, row, column):
        if 0 <= row <= 7 and 0 <= column <= 7:
            self._row = row
            self._col = column
        else:
            raise ValueError(f"Queen(row, column) not on board")

    def can_attack(self, other):
        if self._row == other._row and self._col == other._col:
            raise ValueError("The other queen has same position")
        return (
            self._row == other._row
            or self._col == other._col
            or abs(self._col - other._col) == abs(self._row - other._row)
        )
