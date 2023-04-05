"""An overengineered solution with enums, classes and whatnot."""

from enum import IntEnum


def label(colors):
    """Exercise interface function"""

    return str(Resistor.from_label(colors))


class Unit(IntEnum):
    """Resistor units."""

    OHMS = 0
    KILOOHMS = 3
    MEGAOHMS = 6
    GIGAOHMS = 9

    @classmethod
    def closest_less_or_equal(cls, value):
        return next(m for m in reversed(cls.__members__.values()) if m <= value)


class Code(IntEnum):
    """Resistor color codes."""

    @classmethod
    def _missing_(cls, value):
        if isinstance(value, str):
            for member in cls:
                if member._name_ == value.upper():
                    return member
        return None

    BLACK = 0
    BROWN = 1
    RED = 2
    ORANGE = 3
    YELLOW = 4
    GREEN = 5
    BLUE = 6
    VIOLET = 7
    GREY = 8
    WHITE = 9


class Resistor:
    def __init__(self, value, unit: Unit):
        self.value = value
        self.unit = unit

    @classmethod
    def from_label(cls, label_colors):
        value = Code(label_colors[0])
        exponent = Code(label_colors[2])
        if Code(label_colors[1]) == Code.BLACK:
            exponent += 1
        else:
            value = 10 * value + Code(label_colors[1])
        unit = Unit.closest_less_or_equal(exponent)
        value *= 10 ** (exponent - unit)
        return cls(value, unit)

    def __str__(self):
        return f"{self.value} {self.unit._name_.lower()}"
