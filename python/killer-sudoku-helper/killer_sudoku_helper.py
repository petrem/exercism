"""Killer Sudoku Helper

Return all valid number combinations for a box.

"""
import itertools


def combinations(target: int, size: int, exclude: list[int]) -> list[list[int]]:
    """Bruteforce solution."""
    valid_numbers = set(range(1, min(target + 1, 10))).difference(exclude)
    return sorted(
        list(xs)
        for xs in itertools.combinations(valid_numbers, size)
        if sum(xs) == target
    )
