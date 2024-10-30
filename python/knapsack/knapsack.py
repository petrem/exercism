# Obligatory ;-) exhaustive solution
from functools import reduce
from itertools import combinations


def maximum_value(maximum_weight, items):
    return max(
        (totals[1]
         for j in range(1, len(items))
         for c in combinations(items, j)
         if (totals := reduce(lambda acc, i: (i["weight"] + acc[0], i["value"] + acc[1]), c, (0, 0)))[0] <= maximum_weight),
        default=0)
