"""Translate Human English into Piglet English."""


def translate(text):
    """Rewrite words in text into Piglish."""

    words = text.split()
    return " ".join(f"{_igmepay(word)}ay" for word in words)


_VOWELS = "aeiouy"


def _igmepay(word: str) -> str:
    """Rewrite a word as Piglish."""
    word_len = len(word)
    if word_len <= 1:
        return word
    first = word[0]
    # "?y" words: my -> ym(ay)
    if word_len == 2 and word[1] == "y":
        return f"y{first}"
    # vocal sounds: aardvark -> ardvarka(ay) (???)
    if (
        first in "aeiou"
        or (first == "x" and word[1] == "r")
        or (first == "y" and word[1] == "t")
    ):
        prefix, suffix = word, ""
    else:
        # consonant groups: string -> ingstr(ay)
        try:
            split_at = next(i for i in range(1, word_len - 1) if word[i] in _VOWELS)
            if word[split_at] == "u" and word[split_at - 1] == "q":
                # include "qu" in consonants group
                split_at += 1
            prefix, suffix = word[split_at:], word[:split_at]
        except StopIteration:
            # all consonants, must be pig-polish or something
            prefix, suffix = word, ""
    return "".join((prefix, suffix))
