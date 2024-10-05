from collections import deque
from collections.abc import Iterable
from itertools import chain


def flatten(iterable):
    return list(_flatten4(iterable))

def flatten_filter_none(iterable):
    """Used where the helper generator does not filter None."""
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


def _flatten4(iterable):
    """Non-recursive generator, chaining.
    """
    iterable=iter(iterable)
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

