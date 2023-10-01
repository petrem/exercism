"""Perfect numbers."""


def classify(number):
    """Classify ``number`` as perfect, abundant or deficient."""

    if number <= 0:
        raise ValueError("Classification is only possible for positive integers.")
    return ["perfect", "abundant", "deficient"][
        _cmp(sum(x for x in range(1, number - 1) if number % x == 0), number)
    ]


def _cmp(x, y):
    return (x > y) - (y > x)
