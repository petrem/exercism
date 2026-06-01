"""For simplicity, I'm assuming correct input -- no dangling lines,
no stray corners.
"""

from collections.abc import Iterable
from itertools import chain, combinations


def rectangles(strings):
    found = 0
    potentials = []
    for line in strings:
        vertical_lines = [pos for pos, char in enumerate(line) if char in "+|"]
        sides = list(_horizontal_sides(line))
        for side in potentials:
            if side in sides:
                # count finished rectangles
                found += 1
        # eliminate potentials without vertical continuation
        potentials = [
            side
            for side in potentials
            if side[0] in vertical_lines and side[1] in vertical_lines
        ]
        # add new potential squares
        potentials.extend(sides)
    return found


def _horizontal_sides(line) -> Iterable[tuple[int, int]]:
    def _horizontal_segments() -> Iterable[tuple[int, str]]:
        right = 0
        while (left := line.find("+", right)) >= 0:
            right = line.find(" ", left + 1)
            if right < 0:
                yield left, line[left:]
                break
            yield left, line[left:right]
            left = right + 1

    return chain.from_iterable(
        combinations(
            (start + pos for pos, char in enumerate(segment) if char == "+"), 2
        )
        for start, segment in _horizontal_segments()
    )
