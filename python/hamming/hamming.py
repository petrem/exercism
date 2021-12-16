from itertools import starmap
from operator import ne


def distance(strand_a, strand_b):
    if len(strand_a) != len(strand_b):
        raise ValueError("Undefined for strands of different length.")
    return sum(starmap(ne, zip(strand_a, strand_b)))
