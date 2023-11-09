"""Exercise: word-count"""
from collections import Counter as C
from re import findall


def count_words(ws):
    """Count words in ``sentence``."""
    return C(filter(bool, (w.strip("'") for w in findall(r"[a-z\d']+", ws.lower()))))
