"""Translate Human English into Piglet English."""


def translate(text):
    """Rewrite words in ``text`` into Piglish."""

    return " ".join(_igmepay(word) for word in text.split())


_VOWELS = frozenset("aeiou")
_VOWELS_PLUS = frozenset("aeiouy")


def _igmepay(word: str) -> str:
    """Rewrite a word as Piglish."""
    if any(
        (
            (length := len(word)) == 1,
            (first := word[0]) in _VOWELS,
            # Faster than e.g. `word[:2] in {"xr", "yt"}`
            first == "x" and word[1] == "r",
            first == "y" and word[1] == "t",
        )
    ):
        return word + "ay"
    split_at = next((i for i in range(1, length) if word[i] in _VOWELS_PLUS), length)
    if split_at != length and word[split_at] == "u" and word[split_at - 1] == "q":
        # include "qu" in consonants group
        split_at += 1
    return "".join([word[split_at:], word[:split_at], "ay"])
