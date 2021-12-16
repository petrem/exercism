import re
from itertools import groupby, repeat


def decode(string):
    return "".join(
        c for c in (
            c
            for match in re.finditer(
                r'(?P<repeat>\d+)?(?P<char>[A-Z\s])', string, flags=re.I
            )
            for c in _decode_repeated(match["char"], match["repeat"])
        )
    )


def encode(string):
    return "".join(_encode_repeated(c, len(list(g))) for c, g in groupby(string))


def _decode_repeated(char, char_repeat):
    return char if char_repeat is None else repeat(char, int(char_repeat))


def _encode_repeated(char, char_repeat):
    return f"{char_repeat}{char}" if char_repeat > 1 else char
