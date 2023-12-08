"""Exercise: beer-song."""
from itertools import chain


def recite(start: int, take: int = 1) -> list[str]:
    """Recite bottle song stanzas."""
    stanzas = (_stanza(n) for n in range(start, start - take, -1))
    return list(chain(*_intersperse(("",), stanzas)))


def _stanza(n: int) -> list[str]:
    v1 = f"{_NUMERALS[n]} green bottle{_plural(n)} hanging on the wall,".capitalize()
    return [
        *([v1] * 2),
        "And if one green bottle should accidentally fall,",
        f"There'll be {_NUMERALS[n-1]} green bottle{_plural(n-1)} hanging on the wall.",
    ]


_NUMERALS = [
    "no",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
]


def _plural(n):
    return "s" if n != 1 else ""


def _intersperse(sep, iterable):
    iterator = iter(iterable)
    yield next(iterator)  # pylint: disable=stop-iteration-return
    for e in iterator:
        yield sep
        yield e
