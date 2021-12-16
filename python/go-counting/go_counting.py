from collections import defaultdict, deque

BLACK = "B"
WHITE = "W"
TRANSPARENT = " "
GREY = "G"
NONE = ""


class BW():
    COLORS = {
        "W": WHITE,
        "B": BLACK,
        "G": GREY,
        " ": TRANSPARENT
    }

    def __init__(self, color=TRANSPARENT):
        if color not in BW.COLORS:
            raise ValueError(f"No pretty colors, please: {color}")
        self.color = color

    def __add__(self, other):
        if not isinstance(other, BW):
            raise ValueError(f"Cannot add with {type(other)}")
        if self.color == other.color:
            return self
        elif self.color is TRANSPARENT:
            return other
        elif other.color is TRANSPARENT:
            return self
        else:
            return BW(GREY)

    def __iadd__(self, other):
        self.color = (self + other).color
        return self

    def __str__(self):
        return self.color

    def __repr__(self):
        return f"BW({self.color})"

    def black_or_white(self) -> str:
        if self.color in (BLACK, WHITE):
            return self.color
        else:
            return NONE


class Board:
    """Count territories of each player in a Go game

    Args:
        board (list[str]): A two-dimensional Go board
    """

    def __init__(self, board):
        self.board = board
        self.cols = len(board[0])
        self.lines = len(board)

    def territory(self, x, y):
        """Find the owner and the territories given a coordinate on
           the board

        Args:
            x (int): Column on the board
            y (int): Row on the board

        Returns:
            (str, set): A tuple, the first element being the owner
                        of that area.  One of "W", "B", "".  The
                        second being a set of coordinates, representing
                        the owner's territories.
        """
        if not self.valid_coordinates(x, y):
            raise ValueError(f"Invalid coordinates: ({x}, {y})")
        if self.board[y][x] != TRANSPARENT:
            return NONE, set()
        todo = deque()
        color = BW()
        territory = set()
        done = set()
        todo.append((x, y))
        while todo:
            print("todo:", todo)
            print("territory:", territory)
            print("board:", self.board)
            print("color:", color, type(color))
            i, j = todo.pop()
            if (i, j) in territory:
                print(f"({i},{j}) already in territory")
                continue
            territory.add((i, j))
            done.add((i, j))
            print("neighbors:", self.neighbors(i, j))
            for k, l in self.neighbors(i, j):
                if self.board[l][k] == " ":
                    if (k, l) not in done:
                        print(f"({k},{l}) to be explored")
                        todo.append((k, l))
                else:
                    print(f"({k},{l}) is a margin: {self.board[l][k]}")
                    color += BW(self.board[l][k])

        print("finally", color, territory)
        return color.black_or_white(), territory

    def territories(self):
        """Find the owners and the territories of the whole board

        Args:
            none

        Returns:
            dict(str, set): A dictionary whose key being the owner
                        , i.e. "W", "B", "".  The value being a set
                        of coordinates owned by the owner.
        """
        all_empty_slots = {
            (i, j)
            for i in range(self.cols)
            for j in range(self.lines)
            if self.board[j][i] == TRANSPARENT
        }
        territories = defaultdict(set)
        while True:
            if len(all_empty_slots) == 0:
                break
            i, j = all_empty_slots.pop()
            stone, territory = self.territory(i, j)
            territories[stone].update(territory)
            all_empty_slots.difference_update(territory)
        return territories

    def neighbors(self, x, y):
        return [
            (i, j)
            for (i, j) in [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
            if self.valid_coordinates(i, j)
        ]

    def valid_coordinates(self, x, y):
        return 0 <= x < self.cols and 0 <= y < self.lines
