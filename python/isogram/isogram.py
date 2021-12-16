from collections import Counter
import string


# Make a translation table to remove whitespace and hyphen
TRANS_WSH = str.maketrans("", "", string.whitespace + "-")


def is_isogram(string):
    counted = Counter(string.lower().translate(TRANS_WSH))
    return len(counted) == 0 or counted.most_common(1)[0][1] == 1
