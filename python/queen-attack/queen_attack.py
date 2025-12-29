# pylint: disable=too-few-public-methods
class Queen:
    def __init__(self, row, column):
        self.row = row
        self.col = column
        self._validate()

    def _validate(self):
        if self.row > 7:
            raise ValueError("row not on board")
        if self.row < 0:
            raise ValueError("row not positive")
        if self.col > 7:
            raise ValueError("column not on board")
        if self.col < 0:
            raise ValueError("column not positive")

    def can_attack(self, other):
        if self.row == other.row and self.col == other.col:
            raise ValueError("Invalid queen position: both queens in the same square")
        return (
            self.row == other.row
            or self.col == other.col
            or abs(self.col - other.col) == abs(self.row - other.row)
        )
