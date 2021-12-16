from itertools import combinations, starmap


def two_by_two(sides):
    return starmap(
        lambda x, y: (sides[x], sides[y], sides[3 - x - y]),
        combinations(range(3), 2)
    )


def _is_triangle(sides):
    return all(s > 0 for s in sides) and all(i + j > k for i, j, k in two_by_two(sides))


def is_equilateral(sides):
    return sides[0] > 0 and set(sides) == set([sides[0]])


def is_isosceles(sides):
    return _is_triangle(sides) and any(i == j for i, j, k in two_by_two(sides))


def is_scalene(sides):
    return _is_triangle(sides) and not is_isosceles(sides)
