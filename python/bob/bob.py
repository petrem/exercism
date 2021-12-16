def is_question(phrase):
    return phrase.endswith("?")


def is_shout(phrase):
    # I just learned isupper() ignores non-alpha characters -- weird!
    return phrase.isupper()


def is_silence(phrase):
    return phrase == ""


def hey(phrase):
    cooked_phrase = phrase.strip()
    if is_silence(cooked_phrase):
        return "Fine. Be that way!"
    if is_shout(cooked_phrase):
        if is_question(cooked_phrase):
            return "Calm down, I know what I'm doing!"
        else:
            return "Whoa, chill out!"
    if is_question(cooked_phrase):
        return "Sure."
    return "Whatever."
