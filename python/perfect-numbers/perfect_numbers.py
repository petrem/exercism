def cmp(x, y):
    return (x > y) - (y > x)


def classify(number):
    if number <= 0:
        raise ValueError("Cannot classify negative or zero number")
    return ["perfect", "abundant", "deficient"][
        cmp(
            sum(x for x in range(1, number - 1) if number % x == 0),
            number
        )
    ]
