from collections import Counter
import re


def count_words(sentence):
    return dict(
        Counter(
            word.lower().strip("'") for word in re.findall(r"[A-Za-z0-9']+", sentence)
        )
    )
