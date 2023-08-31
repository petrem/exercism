"""triangle"""
from itertools import combinations, starmap


def _two_by_two(sides):
    return starmap(
        lambda x, y: (sides[x], sides[y], sides[3 - x - y]), combinations(range(3), 2)
    )


def _is_triangle(sides):
    return all(s > 0 for s in sides) and all(
        i + j > k for i, j, k in _two_by_two(sides)
    )


def equilateral(sides):
    """Is equilateral?"""
    return sides[0] > 0 and set(sides) == {sides[0]}


def isosceles(sides):
    """Is isosceles?"""
    return _is_triangle(sides) and any(i == j for i, j, k in _two_by_two(sides))


def scalene(sides):
    """Is scalene?"""
    return _is_triangle(sides) and not isosceles(sides)
