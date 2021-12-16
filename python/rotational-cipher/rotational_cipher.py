# adapted from https://exercism.io/tracks/python/exercises/rotational-cipher/solutions/d30f637d8c1044ab948c6a2998be2795
# please credit the linked solution, not this one


from string import ascii_lowercase, ascii_letters


_repeated_lowercase = ascii_lowercase + ascii_lowercase


def rotate(text, key):
    rotated = _repeated_lowercase[key:key + 26]
    trans = str.maketrans(ascii_letters, rotated + rotated.upper())
    return text.translate(trans)


# my original schoolbook solution

def rotate1(text: str, key: int) -> str:
    return "".join(_rotate_char(c, key) for c in text)


def _rotate_char(c: str, key: int) -> str:
    if c.isascii() and c.isalpha():
        displacement = ord("A") if c.isupper() else ord("a")
        return chr((ord(c) - displacement + key) % 26 + displacement)
    return c
