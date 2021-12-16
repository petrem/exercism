from itertools import accumulate, repeat, starmap


def encode(numbers):
    return [byte for number in numbers for byte in _encode(number)]


def decode(bytes_):
    return [_decode(chunk) for chunk in _chunks(bytes_)]


def _encode(number):
    encoder = starmap(
        second,
        takeuntil(
            lambda t: first(*t) == 0,
            # as accumulate's first returned element will be the number itself, skip it
            tail(
                accumulate(
                    repeat((number, None)),
                    lambda t, _: (first(*t) >> 7, first(*t) & 0x7f)
                )
            )
        )
    )
    last = next(encoder)
    return reversed([last] + [byte | 0x80 for byte in encoder])


def _decode(bytes_):
    if bytes_[-1] & 0x80:
        raise ValueError(f"Incomplete sequence: {bytes_}")
    return sum((byte & 0x7f) * 0x80 ** i for i, byte in enumerate(reversed(bytes_)))


def _chunks(bytes_):
    it = iter(bytes_)
    while True:
        chunk = list(takeuntil(_is_terminal, it))
        if chunk:
            yield chunk
        else:
            break


def _is_terminal(byte):
    return not byte & 0x80


def takeuntil(pred, iterable):
    """Similar to itertools.takewhile returns up until and including element
    matching the predicate.

    takeuntil(lambda x: x != 0, [3,2,1,0,0,0]) --> [3,2,1,0]
    """
    for value in iterable:
        yield value
        if pred(value):
            break


def first(t1, t2):
    return t1


def second(t1, t2):
    return t2


def tail(iterable):
    it = iter(iterable)
    next(it)
    yield from it
