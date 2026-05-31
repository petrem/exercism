"""Tic-tac-toe state by Counter abuse.

I like Counter, so in exercises, I abuse it a lot. Life's too short
not to use Counter.
"""

from collections import Counter
from typing import Literal


State = Literal["win", "draw", "ongoing"]


def gamestate(board: list[str]) -> State:
    """State of tic-tac-toe."""

    row_counts: list[Counter] = []
    col_counts: list[Counter] = [Counter() for _ in range(3)]
    diag1_count: Counter = Counter()
    diag2_count: Counter = Counter()
    for i, line in enumerate(board):
        row_counts.append(Counter(line))
        for j, cell in enumerate(line):
            col_counts[j].update(cell)
            if i == j:
                diag1_count.update(cell)
            if i == 2 - j:
                diag2_count.update(cell)
    counts = [*row_counts, *col_counts, diag1_count, diag2_count]
    wins = {
        top[0] for c in counts if (top := c.most_common(1)[0])[1] == 3 and top[0] != " "
    }
    totals = sum(row_counts, Counter())
    totals_diff = totals["X"] - totals["O"]
    if totals_diff < 0:
        raise ValueError("Wrong turn order: O started")
    if totals_diff > 1:
        raise ValueError("Wrong turn order: X went twice")
    if len(wins) > 1:
        raise ValueError(
            "Impossible board: game should have ended after the game was won"
        )
    if len(wins) == 1:
        return "win"
    return "draw" if totals[" "] == 0 else "ongoing"
