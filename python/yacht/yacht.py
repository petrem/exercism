from collections import Counter
from functools import partial


def _n_of_a_kind(n, op, dice):
    which, how_many = Counter(dice).most_common(1)[0]
    if how_many >= n:
        return op(which)
    return 0


YACHT = partial(_n_of_a_kind, 5, lambda _: 50)


def _score_n(n, dice):
    return n * Counter(dice).get(n, 0)


ONES = partial(_score_n, 1)
TWOS = partial(_score_n, 2)
THREES = partial(_score_n, 3)
FOURS = partial(_score_n, 4)
FIVES = partial(_score_n, 5)
SIXES = partial(_score_n, 6)


def FULL_HOUSE(dice):
    if sorted(Counter(dice).values()) == [2, 3]:
        return sum(dice)
    return 0


FOUR_OF_A_KIND = partial(_n_of_a_kind, 4, lambda which: 4 * which)


def _straight(start, dice):
    if sorted(dice) == list(range(start, start+5)):
        return 30
    return 0


LITTLE_STRAIGHT = partial(_straight, 1)
BIG_STRAIGHT = partial(_straight, 2)


def CHOICE(dice):
    return sum(dice)


def score(dice, category):
    return category(dice)
