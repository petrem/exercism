from math import inf, sqrt


def score(x, y):
    d = sqrt(x*x + y*y)
    circle = next(filter(lambda e: d <= e[0], [(1, 10), (5, 5), (10, 1), (inf, 0)]))
    return circle[1]
