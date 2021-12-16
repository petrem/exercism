"""Let's use classes, why not."""

from __future__ import annotations
import typing
from string import ascii_uppercase


def rows(letter):
    return Diamond(letter).as_list()


class Diamond:
    def __init__(self, letter: str) -> None:
        if _isletter(letter):
            self.letter = letter.upper()
            self.width = _offset(letter) * 2 + 1
        else:
            raise ValueError(f"{letter} is not a letter")

    def __str__(self) -> str:
        return "\n".join(self.as_list())

    def as_list(self) -> typing.List[str]:
        letters = (
            ascii_uppercase[:_offset(self.letter)]
            + ascii_uppercase[_offset(self.letter)::-1]
        )
        return [f"{_make_diamond_line(letter)!s:^{self.width}}" for letter in letters]


def _make_diamond_line(letter: str) -> str | _DiamondLine:
    if letter == "A":
        return "A"
    return _DiamondLine(letter)


class _DiamondLine:
    def __init__(self, letter: str) -> None:
        assert _isletter(letter)
        self.letter = letter

    def __str__(self) -> str:
        mid_space = 2 * _offset(self.letter) - 1
        return f"{self.letter}{'':{mid_space}}{self.letter}"


def _offset(c: str) -> int:
    return ord(c) - ord("A")


def _isletter(letter: typing.Any) -> bool:
    return (
        isinstance(letter, str)
        and len(letter) == 1
        and letter.isascii()
        and letter.isalpha()
    )
