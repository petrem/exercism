"""A nicely overengineered solution *abusing* `match`.

Yes I know we could just play with strings, more or less.

"""
from enum import IntEnum, StrEnum
from functools import reduce


def resistor_label(colors):
    return str(Resistor.from_label(colors))


class Unit(IntEnum):
    """Resistence units."""

    OHMS = 0
    KILOOHMS = 3
    MEGAOHMS = 6
    GIGAOHMS = 9

    @classmethod
    def closest_less_or_equal(cls, value):
        return next(m for m in reversed(cls.__members__.values()) if m <= value)


class NameLookupMixin:
    """Allow lookup by case-insensitive name for enums."""
    @classmethod
    def _missing_(cls, value):
        upper_value = value.upper()
        if isinstance(value, str):
            return next(
                (member for member in cls if member._name_ == upper_value), None
            )
        return None


class ResistanceColor(NameLookupMixin, IntEnum):
    """Resistor color codes."""
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


class ToleranceColor(NameLookupMixin, StrEnum):
    """Resistor tolerance percentage color codes."""
    GREY = "0.05"
    VIOLET = "0.1"
    BLUE = "0.25"
    GREEN = "0.5"
    BROWN = "1"
    RED = "2"
    GOLD = "5"
    SILVER = "10"


class Resistor:
    """Represent a resistor with a floating-point value, tolerance, etc.

    All the conversion from int to float, precision, etc., is largely onerous.
    Let's suppose that's what we need. Just play along.
    You should not approach the exercise like this, though.
    """
    def __init__(
        self,
        value: int | float,
        unit: Unit = Unit.OHMS,
        tolerance: str | None = None,
        decimals: int | None = None,
    ):
        self.value = value
        self.unit = unit
        self.tolerance = tolerance
        self.decimals = decimals

    @classmethod
    def from_label(cls, colors):
        match colors:
            case [color] if (value := ResistanceColor(color)) == ResistanceColor.BLACK:
                return cls(value)
            case [*bands, multiplier, tolerance] if 2 <= len(bands) <= 3:
                exponent = ResistanceColor(multiplier)
                bands = [ResistanceColor(band) for band in bands]
                match bands:
                    case [_, ResistanceColor.BLACK]:
                        exponent += 1
                        n_digits = 1
                    case [_, ResistanceColor.BLACK, ResistanceColor.BLACK]:
                        exponent += 2
                        n_digits = 1
                    case [_, _, ResistanceColor.BLACK]:
                        exponent += 1
                        n_digits = 2
                    case _:
                        n_digits = len(bands)
                value = reduce(
                    lambda acc, band: band + acc * 10, bands[:n_digits], 0
                )
                unit = Unit.closest_less_or_equal(exponent + n_digits - 1)
                correction = exponent - unit
                value *= 10 ** (correction)
                return cls(value, unit, ToleranceColor(tolerance), decimals=-correction)
            case _:
                raise ValueError(f"Invalid colors: {colors}")

    def __str__(self):
        tolerance_str = f" ±{self.tolerance}%" if self.tolerance else ""
        unit_str = self.unit._name_.lower()
        if isinstance(self.value, int):
            precision = ".0"
        elif self.decimals is None:
            precision = ""
        else:
            precision = f".{self.decimals}"
        return f"{self.value:{precision}f} {unit_str}{tolerance_str}"
