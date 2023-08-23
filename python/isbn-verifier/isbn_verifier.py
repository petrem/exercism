"""The solution iterates through the string just once. Otherwise, not particularly nice."""
from itertools import count, islice
from operator import mul


def is_valid(isbn):
    """Validate ISBN-10 string."""
    digits = (d for d in isbn if d != "-")
    counter = count(10, -1)
    try:
        checksum = sum(map(mul, (int(d) for d in islice(digits, 9)), counter))
        control = next(digits)
        checksum += 10 if control == "X" else int(control)
        if next(counter) != 1 or next(digits, None) is not None:
            return False
    except (StopIteration, ValueError):
        return False
    return checksum % 11 == 0
