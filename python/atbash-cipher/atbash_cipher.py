from string import ascii_lowercase, digits
from functools import partial
from itertools import zip_longest


def encode(plain_text, groups_of=5):
    return " ".join(
        "".join(group)
        for group in _atbash_grouper(filter(str.isalnum, plain_text.lower()))
    ).translate(_TRANS_ENCODE)


def decode(ciphered_text):
    return ciphered_text.translate(_TRANS_DECODE)


def _grouper(n, iterable, padvalue=None):
    "grouper(3, 'abcdefg', 'x') --> ('a','b','c'), ('d','e','f'), ('g','x','x')"
    return zip_longest(*[iter(iterable)] * n, fillvalue=padvalue)


_atbash_grouper = partial(_grouper, 5, padvalue="")


_TRANS_ENCODE = str.maketrans(
    ascii_lowercase + digits,
    ascii_lowercase[::-1] + digits,
)

_TRANS_DECODE = str.maketrans(
    ascii_lowercase + digits,
    ascii_lowercase[::-1] + digits,
    " "
)
