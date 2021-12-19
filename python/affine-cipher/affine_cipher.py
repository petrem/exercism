"""Encode and decode using affine cipher."""
import string
from functools import partial
from itertools import zip_longest
from typing import Callable, Tuple

ALPHABET = string.ascii_lowercase
_ALPHABET_LENGTH = len(ALPHABET)


def encode(plain_text: str, a: int, b: int) -> str:
    g, _, _ = _xgcd(a, _ALPHABET_LENGTH)
    if g != 1:
        raise ValueError("a and m must be coprime.")
    return " ".join(
        "".join(group)
        for group in _affine_grouper(
            _transcode(c, _encrypt_char, a, b) for c in plain_text if c.isalnum()
        )
    )


def decode(ciphered_text: str, a: int, b: int) -> str:
    return "".join(
        _transcode(c, _decrypt_char, a, b) for c in ciphered_text if c.isalnum()
    )


def _transcode(char: str, fn: Callable[[int, int, int], int], a: int, b: int) -> str:
    if char.isalpha():
        char_index = ALPHABET.index(char.lower())
        transcoded_index = fn(char_index, a, b)
        return ALPHABET[transcoded_index]
    if char.isdigit():
        return char
    raise RuntimeError(f"Shouldn't have called _transcode with {char}!")


def _encrypt_char(x: int, a: int, b: int) -> int:
    return (a * x + b) % _ALPHABET_LENGTH


def _decrypt_char(y: int, a: int, b: int) -> int:
    try:
        return _modinv(a, _ALPHABET_LENGTH) * (y - b) % _ALPHABET_LENGTH
    except ModularInverseError as e:
        raise ValueError("a and m must be coprime.") from e


# From: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
def _xgcd(a: int, b: int) -> Tuple[int, int, int]:
    """return (g, x, y) such that a*x + b*y = g = gcd(a, b)"""
    x0, x1, y0, y1 = 0, 1, 1, 0
    while a != 0:
        (q, a), b = divmod(b, a), a
        y0, y1 = y1, y0 - q * y1
        x0, x1 = x1, x0 - q * x1
    return b, x0, y0


class ModularInverseError(Exception):
    pass


def _modinv(a: int, b: int) -> int:
    """return x such that (x * a) % b == 1"""
    g, x, _ = _xgcd(a, b)
    if g != 1:
        raise ModularInverseError("gcd(a, b) != 1")
    return x % b


def _grouper(n, iterable, padvalue=None):
    "grouper(3, 'abcdefg', 'x') --> ('a','b','c'), ('d','e','f'), ('g','x','x')"
    return zip_longest(*[iter(iterable)] * n, fillvalue=padvalue)


_affine_grouper = partial(_grouper, 5, padvalue="")
