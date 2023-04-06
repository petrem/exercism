"""An alternative solution, playing with strings. Don't use in production! ;-)"""


def label(colors):
    codes = [COLOR_CODES[c] for c in colors]
    ohms = "".join(
        (
            str(codes[0]) if codes[0] else "",
            str(codes[1]),
            "0" * codes[2],
        )
    )
    if ohms.endswith("0"):
        pivot = ohms.find("0")
        unit_prefix, rest = divmod(len(ohms) - pivot, 3)
        significant = pivot + rest
    else:
        unit_prefix = 0
        significant = len(ohms)
    return f"{ohms[:significant]} {UNIT_PREFIXES[unit_prefix]}ohms"


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
