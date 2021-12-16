from copy import copy
from functools import wraps


def _raiseIfEmpty(fn):
    @wraps(fn)
    def wrapper(self, *args, **kwargs):
        if len(self) == 0:
            raise EmptyListException("Can't do that to an empty list")
        return fn(self, *args, **kwargs)
    return wrapper


class LinkedList:
    def __init__(self, values=[]):
        self._len = len(values)
        self._value = values[-1] if values else None
        self._next = LinkedList(values[:-1]) if len(values) > 1 else None

    def value(self):
        return self._value

    def next(self):
        return self._next

    def __len__(self):
        return self._len

    def __iter__(self):

        def iterator(current):
            while current is not None and len(current) > 0:
                value = current._value
                current = current._next
                yield value
        return iterator(self)

    @_raiseIfEmpty
    def head(self):
        return self

    def push(self, value):
        self._next = copy(self)
        self._value = value
        self._len += 1

    @_raiseIfEmpty
    def pop(self):
        value = self._value
        if self._next:
            self._len = self._next._len
            self._value = self._next._value
            self._next = self._next._next
        else:
            self._value = None
            self._len = 0
        return value

    def reversed(self):
        return LinkedList(list(self))


class EmptyListException(Exception):
    pass
