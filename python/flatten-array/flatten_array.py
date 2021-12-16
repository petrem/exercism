from collections.abc import Iterable
from collections import deque


def flatten(iterable):
    return [x for x in _flatten3(iterable) if x is not None]


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
    while(len(stack) > 0):
        element = stack.popleft()
        if isinstance(element, Iterable):
            stack.extendleft(reversed(list(element)))
        else:
            yield element


def _flatten3(iterable):
    """Non-recursive generator, stacking iterators."""
    iterators = deque()
    iterators.append(iter(iterable))
    while(len(iterators) > 0):
        iterator = iterators.pop()
        for item in iterator:
            if isinstance(item, Iterable):
                iterators.append(iterator)
                iterators.append(iter(item))
                break
            else:
                yield item
