from itertools import count
from operator import mul


def verify(isbn):
    try:
        control = 10 if isbn[-1] == "X" else int(isbn[-1])
    except (ValueError, IndexError):
        return False
    counter = count(10, -1)
    checksum = sum(
        map(
            mul,
            (int(d) for d in isbn[:-1] if d.isdigit()),
            counter,
        )
    ) + control
    if next(counter) != 1:
        return False
    return checksum % 11 == 0
