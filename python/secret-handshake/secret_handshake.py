from functools import reduce


def _append(text):
    def _op(l):
        return l + [text]
    setattr(_op, "text", text)
    return _op


def _reversed():
    def _op(l):
        return reversed(l)
    setattr(_op, "text", "reversed")
    return _op


BIT_COMMANDS = [
    (0b1, _append("wink")),
    (0b10, _append("double blink")),
    (0b100, _append("close your eyes")),
    (0b1000, _append("jump")),
    (0b10000, _reversed()),
]

ACTIONS = {op.text: bit for bit, op in BIT_COMMANDS if hasattr(op, "text")}


def commands(number):
    def perform_command(acc, bit_cmd):
        bit, op = bit_cmd
        return op(acc) if number & bit else acc

    return list(reduce(perform_command, BIT_COMMANDS, []))


def secret_code(actions):
    is_ordered = 0

    def or_checking_order(x, y):
        if x > y:
            nonlocal is_ordered
            is_ordered = ACTIONS["reversed"]
        return x | y

    return reduce(
        or_checking_order, (ACTIONS[action] for action in actions)
    ) | is_ordered
