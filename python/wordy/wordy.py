import operator as op
import re


OPERATIONS = {
    "plus": op.add,
    "minus": op.sub,
    "multiplied by": op.mul,
    "divided by": op.truediv,
}
WORDY_RE = re.compile(
    r"What is\s+(?:(?P<number>\d+)|"
    rf"(?P<expr>\d+\s+[\d\w ]+\s+\d+))\s*\?",
)


def answer(question):
    if m := WORDY_RE.fullmatch(question):
        if number := m.group("number"):
            return int(number)
        if expr := m.group("expr"):
            ...
    raise ValueError()
