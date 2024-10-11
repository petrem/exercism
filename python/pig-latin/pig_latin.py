"""Translate Human English into Piglet English."""

# ruff: noqa: ERA001


def translate(text):
    """Rewrite words in ``text`` into Piglish."""

    return " ".join(_igmepay(word) for word in text.split())


_VOWELS_START = (*tuple("aeiou"), "xr", "yt")
_VOWELS_MIDDLE = frozenset("aeiouy")


def _igmepay(word: str) -> str:
    """Rewrite a word as Piglish."""
    # The following seemd faster last time I looked at this exercise, but
    # this time (py3.12) it is slower.
    # if any(
    #     (
    #         (length := len(word)) == 1,
    #         (first := word[0]) in _VOWELS,
    #         first == "x" and word[1] == "r",
    #         first == "y" and word[1] == "t",
    #     )
    # ):
    if word.startswith(_VOWELS_START):
        return word + "ay"
    length = len(word)
    split_at = next((i for i in range(1, length) if word[i] in _VOWELS_MIDDLE), length)
    # This is still faster then e.g word.find(..) or slicing
    if split_at != length and word[split_at] == "u" and word[split_at - 1] == "q":
        # include "qu" in consonants group
        split_at += 1
    return "".join([word[split_at:], word[:split_at], "ay"])
