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
    n_coins, change = _find_fewest_coins(coins, target)
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
