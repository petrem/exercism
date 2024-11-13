"""Exercise: reverse-string.

With many possibilities -- this is coming from "mentoring" session questions.
"""

from collections import deque


def reverse0(text):
    """Reverse ``text``."""
    return text[::-1]


def reverse1(text):
    """Taking advantage a str is a sequence."""

    return "".join(reversed(text))


def reverse2(text):
    """Taking advantage a str is a sequence, and using a generator expression."""
    return "".join(text[k] for k in reversed(range(len(text))))


def reverse3(iterable):
    """Returning a list.
    Using a  classic for-loop based iteration.
    Inserting elements at the beginning of a list is inefficient.
    Only append is efficient, but we need to reverse.
    """

    accum = []
    for x in iterable:
        accum.insert(0, x)

    # re-creating a string for passing the tests.
    return "".join(accum)


def reverse4(iterable):
    """Returning a list.
    Using a  classic for-loop based iteration.
    Inefficient because it constructs new lists each time.
    """

    accum = []
    for x in iterable:
        accum = [x, *accum]

    # re-creating a string for passing the tests.
    return "".join(accum)


def reverse5(iterable):
    """Returning a deque.
    Using a  classic for-loop based iteration.
    Inserting elements at both begginning and end of a deque is efficient.
    Likely, this is the most efficient except for [::-1] and maybe reversed().
    """

    accum = deque()
    for x in iterable:
        accum.appendleft(x)

    # re-creating a string for passing the tests.
    return "".join(accum)


def reverse6(iterable):
    """Recursive.
    Note that is we receive an iterable, not an iterator, we must call iter() on it.
    The _reverse function does the work, but we get a list (similar to those above)
    so we need to convert back to a string.
    """

    def _reverse(iterator):
        if (head := next(iterator, None)) is None:
            return []
        r = _reverse(iterator)
        r.append(head)
        return r

    return "".join(_reverse(iter(iterable)))


def reverse7(iterable):
    """A generator.
    We still need to hold all the elements somewhere."""

    def _reverse(iterable):
        elements = list(iterable)
        yield from reversed(elements)

    return "".join(_reverse(iterable))


# use this version when running the tests
reverse = reverse7
