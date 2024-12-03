"""Let's use classes, why not."""

import typing
from string import ascii_uppercase


def rows(letter):
    return Diamond(letter).as_list()


class Diamond:
    def __init__(self, letter: str) -> None:
        if _isletter(letter):
            self.letter = letter.upper()
            self.offset = _offset(letter)
            self.width = self.offset * 2 + 1
        else:
            raise ValueError(f"{letter} is not a letter")

    def __str__(self) -> str:
        return "\n".join(self.as_list())

    def as_list(self) -> list[str]:
        letters = ascii_uppercase[: self.offset] + ascii_uppercase[self.offset :: -1]
        return [f"{diamondline(letter)!s:^{self.width}}" for letter in letters]


def diamondline(letter):
    if letter == "A":
        return "A"
    mid_space = " " * (2 * _offset(letter) - 1)
    return f"{letter}{mid_space}{letter}"


def _offset(c: str) -> int:
    return ord(c) - ord("A")


def _isletter(letter: typing.Any) -> bool:
    return (
        isinstance(letter, str)
        and len(letter) == 1
        and letter.isascii()
        and letter.isalpha()
    )
