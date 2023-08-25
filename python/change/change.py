"""Exercism: Change coins."""
import operator
from functools import total_ordering
from itertools import takewhile
from typing import List, Tuple

TCoins = List[int]
TSolution = Tuple[int, TCoins]


def find_fewest_coins(coins: TCoins, target: int) -> TCoins:
    """Find fewest items from `coins` adding up to `target`."""
    if target < 0:
        raise ValueError("target can't be negative")
    n_coins, change = _find_fewest_coins(sorted(coins), target)
    if n_coins is Largest():
        raise ValueError("can't make target with given coins")
    return change


@total_ordering
class Largest:
    """A singleton acting as infinity."""

    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def __lt__(self, other):
        return False

    def __add__(self, other):
        return self

    def __repr__(self):
        return "Largest"


fst = operator.itemgetter(0)
snd = operator.itemgetter(1)


def _make_new_solution(solution, denomination):
    return fst(solution) + 1, [denomination] + snd(solution)


def _find_fewest_coins(coins: TCoins, target: int) -> TSolution:
    solutions: List[TSolution] = [(0, [])]
    solutions.extend(
        _make_new_solution(
            *min(
                (
                    (solutions[j - denomination], denomination)
                    for denomination in takewhile(
                        lambda c: c <= j, coins  # pylint: disable=[cell-var-from-loop]
                    )
                ),
                key=lambda t: t[0][0],
                default=((Largest(), []), 0),
            )
        )
        for j in range(1, target + 1)
    )
    return solutions[-1]

# def _find_coins_dyn(coins: TCoins, target: int) -> TSolution:
#     dyn_matrix = [[None]]

#     def rec(N, m):
#         if N < 0:
#             raise ValueError(f"No solution: negative N: {N}")
#         if N == 0:
#             return []
#         if m <= 0:
#             raise ValueError("No solution: no change left")
#         yield


def print_matrix(M):
    n = len(M)
    m = len(M[0])
    max_val = max(max(line) for line in M)
    width = len(str(max_val)) + 1
    for i in range(n):
        for j in range(m):
            print(f"{M[i][j]:<{width}}", end="")
        print()


def count_dyn(n, S):
    m = len(S)
    table = [[1] * (m + 1)] * (n + 2)
    for i in range(1, n + 2):
        for j in range(0, m + 1):
            if j == 0:
                if i % S[j] == 0:
                    table[i][j] = 1
                else:
                    table[i][j] = 0
            elif S[j] > i:
                table[i][j] = table[i][j - 1]
            else:
                table[i][j] = table[i - S[j]][j] + table[i][j - 1]
    print(table)
    return table[n, m - 1]
