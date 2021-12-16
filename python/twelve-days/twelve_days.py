SPELL_NUMBER = [
    ("a", "first"),
    ("two", "second"),
    ("three", "third"),
    ("four", "fourth"),
    ("five", "fifth"),
    ("six", "sixth"),
    ("seven", "seventh"),
    ("eight", "eighth"),
    ("nine", "ninth"),
    ("ten", "tenth"),
    ("eleven", "eleventh"),
    ("twelve", "twelfth"),
]


def spell(n: int) -> str:
    return SPELL_NUMBER[n - 1][0]


def nth(n: int) -> str:
    return SPELL_NUMBER[n - 1][1]


GIFTS = [
    "Partridge in a Pear Tree",
    "Turtle Doves",
    "French Hens",
    "Calling Birds",
    "Gold Rings",
    "Geese-a-Laying",
    "Swans-a-Swimming",
    "Maids-a-Milking",
    "Ladies Dancing",
    "Lords-a-Leaping",
    "Pipers Piping",
    "Drummers Drumming",
]


def gifts(n: int) -> str:
    and_ = "and " if n > 1 else ""
    return ", ".join(
        reversed([
            ("" if i > 1 else and_) + spell(i) + " " + gift
            for i, gift in enumerate(GIFTS[:n], 1)
        ])
    )


def recite(start_verse: int, end_verse: int) -> [str]:
    return [
        f"On the {nth(n)} day of Christmas my true love gave to me: {gifts(n)}."
        for n in range(start_verse, end_verse + 1)
    ]
