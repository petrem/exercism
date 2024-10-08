from collections import deque
from collections.abc import Iterable
from itertools import chain


def flatten(iterable):
    return [item for item in _flatten3(iterable) if item is not None]


def _flatten1(iterable):
    """Recursive generator. _Very_ deeply nested lists will fail."""
    for x in iterable:
        if isinstance(x, Iterable):
            yield from _flatten1(x)
        else:
            yield x


def _flatten2(iterable):
    """Non-recursive generator, using a stack.

    Expands iterables therefore uses more memory.
    """
    stack = deque(iterable)
    while len(stack) > 0:
        element = stack.popleft()
        if isinstance(element, Iterable):
            stack.extendleft(reversed(list(element)))
        else:
            yield element


def _flatten3(iterable):
    """Non-recursive generator, using a stack but no expanding inner iterators."""
    stack = [iter(iterable)]

    while stack:
        for item in (current := stack.pop()):
            if isinstance(item, Iterable):
                stack.append(current)
                stack.append(iter(item))
                break
            if item is not None:
                yield item


def _flatten4(iterable):
    """Non-recursive generator, chaining."""
    iterable = iter(iterable)
    try:
        while True:
            match next(iterable):
                case None:
                    pass
                case Iterable() as item:
                    iterable = chain(item, iterable)
                case item:
                    yield item
    except StopIteration:
        pass
