"""Collatz conjecture."""


def steps(number):
    """Calculate number of steps taken to get to 1 from ``number``."""

    if number <= 0:
        raise ValueError("Only positive integers are allowed")
    count = 0
    while number > 1:
        if number % 2 == 0:
            number = number // 2
            count += 1
        else:
            number = (3 * number + 1) // 2
            count += 2
    return count
