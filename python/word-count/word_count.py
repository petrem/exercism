"""Exercise: word-count"""
import re
from collections import Counter


def count_words(ws):
    """Count words in ``sentence``."""
    vs = (
        v for w in re.findall(r"[A-Za-z\d']+", ws) if (v := w.lower().strip("'")) != ""
    )
    return Counter(vs)
