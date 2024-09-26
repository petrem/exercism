from collections import Counter


def _n_of_a_kind(n, op, dice):
    which, how_many = Counter(dice).most_common(1)[0]
    return op(which) if how_many >= n else 0


ONES, TWOS, THREES, FOURS, FIVES, SIXES = (lambda dice, n=k: n * dice.count(n) for k in range(1, 7))
YACHT = lambda dice: _n_of_a_kind(5, lambda _: 50, dice)
FOUR_OF_A_KIND = lambda dice: _n_of_a_kind(4, lambda which: 4 * which, dice)
LITTLE_STRAIGHT, BIG_STRAIGHT = (lambda dice, n=k: 30 if sorted(dice) == list(range(n, n+5)) else 0 for k in range(1, 3))
CHOICE = sum
FULL_HOUSE = lambda dice: sum(dice) if sorted(Counter(dice).values()) == [2, 3] else 0


def score(dice, category):
    return category(dice)
