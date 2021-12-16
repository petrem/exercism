from itertools import starmap
from operator import mul


COLORS = [
    'black',
    'brown',
    'red',
    'orange',
    'yellow',
    'green',
    'blue',
    'violet',
    'grey',
    'white'
]


def color_code(color):
    return COLORS.index(color)


def value1(colors):
    return sum(
        starmap(
            mul,
            zip(
                reversed(list(map(color_code, colors))),
                map(lambda x: 10**x, range(0, len(colors)))
            )
        )
    )


# pythonic alternative 1
def value(colors):
    return int("".join(str(color_code(color)) for color in colors))
