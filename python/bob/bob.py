"""Bob."""


def response(phrase):
    """Respond lackadaisically."""
    cooked_phrase = phrase.strip()
    if _is_silence(cooked_phrase):
        return "Fine. Be that way!"
    if _is_shout(cooked_phrase):
        if _is_question(cooked_phrase):
            return "Calm down, I know what I'm doing!"
        return "Whoa, chill out!"
    if _is_question(cooked_phrase):
        return "Sure."
    return "Whatever."


def _is_question(phrase):
    return phrase.endswith("?")


def _is_shout(phrase):
    # I just learned isupper() ignores non-alpha characters -- weird!
    return phrase.isupper()


def _is_silence(phrase):
    return phrase == ""
