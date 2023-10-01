"""Raindrops."""
from collections import namedtuple


RainSpeak = namedtuple("RainSpeak", "factor, translation")
RAINSPEAK = [
    RainSpeak(3, "Pling"),
    RainSpeak(5, "Plang"),
    RainSpeak(7, "Plong"),
]


def convert(number):
    """Convert to raindrop-speak."""

    return (
        "".join(rs.translation for rs in RAINSPEAK if number % rs.factor == 0)
        or f"{number}"
    )
