"""Let's have a more decent one, too. Not!!"""


def label(colors: list[str]) -> str:
    """Read ``colors`` label on a resistor."""
    codes = [COLOR_CODES[c] for c in colors]
    resistence = f"{codes[0]}{codes[1]}{'0' * codes[2]}".lstrip("0")
    significant, *groups = resistence.rsplit("000")
    return f"{significant or '0'} {UNIT_PREFIXES[len(groups)]}ohms"


COLOR_CODES = {
    c: j
    for j, c in enumerate(
        [
            "black",
            "brown",
            "red",
            "orange",
            "yellow",
            "green",
            "blue",
            "violet",
            "grey",
            "white",
        ]
    )
}


UNIT_PREFIXES = ["", "kilo", "mega", "giga"]
