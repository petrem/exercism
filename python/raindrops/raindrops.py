from collections import namedtuple


RainSpeak = namedtuple("RainSpeak", "factor, translation")
RAINSPEAK = [
    RainSpeak(3, "Pling"),
    RainSpeak(5, "Plang"),
    RainSpeak(7, "Plong"),
]


def raindrops(number):
    translation = "".join(rs.translation for rs in RAINSPEAK if number % rs.factor == 0)
    if translation:
        return translation
    return f"{number}"
