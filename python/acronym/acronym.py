import re


def abbreviate1(words):
    def capitalized_first_letter(word):
        try:
            return next(c for c in word if c.isalpha()).upper()
        except StopIteration:
            pass

    return "".join(
        filter(None, (
            capitalized_first_letter(word)
            for dashed_word in words.replace("'s ", " ").replace("' ", " ").split()
            for word in dashed_word.split("-")
        ))
    )


# OR

def abbreviate2(words):
    def isalpha_or_quote(s):
        return s.isalpha() or s == "'"

    return (words[0].upper() if words[0].isalpha() else "") + "".join(
        b.upper()
        for a, b in zip(words[:-1], words[1:])
        if not isalpha_or_quote(a) and isalpha_or_quote(b)
    )


# OR

def abbreviate3(words):
    return "".join(
        map(str.upper, re.findall(r"(?<![a-z'])([a-z])", words, re.I))
    )


abbreviate = abbreviate1
