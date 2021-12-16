from functools import wraps

# A variant with no separate Node class. For kicks.


class EmptyListException(Exception):
    pass


def _raise_if_empty(fn):
    @wraps(fn)
    def wrapper(self, *args, **kwargs):
        if len(self) == 0:
            raise EmptyListException("Can't do that to an empty list")
        return fn(self, *args, **kwargs)
    return wrapper


class LinkedList:
    """A circular double-linked list."""

    EMPTY = object()

    def __init__(self):
        self._next = self
        self._prev = self
        self._value = LinkedList.EMPTY

    @classmethod
    def make_linked_list_head(cls, value, prev, next_):
        self = cls()
        self._value = value
        self._prev = prev
        self._next = next_
        return self

    def __len__(self):
        return len(list(iter(self)))

    def push(self, value):
        if self._value is LinkedList.EMPTY:
            self._value = value
        else:
            prev = self._prev
            new = self.make_linked_list_head(value, prev=prev, next_=self)
            prev._next = new
            self._prev = new

    def unshift(self, value):
        if self._value is LinkedList.EMPTY:
            self._value = value
        else:
            prev = self._prev
            next_ = self._next
            # make a "clone" of the head
            new = self.make_linked_list_head(self._value, self, self._next)
            # make head the new node
            self._value = value
            self._next = new
            if prev is self:
                self._prev = new
            else:
                next_._prev = new
                prev._next = self

    @_raise_if_empty
    def pop(self):
        prev = self._prev
        value = prev._value
        if prev is self:
            self._value = LinkedList.EMPTY
        else:
            self._prev = prev._prev
            prev._next = self
        return value

    @_raise_if_empty
    def shift(self):
        next_ = self._next
        value = self._value
        if next_ is self:
            self._value = LinkedList.EMPTY
        else:
            next_next = next_._next
            self._value = next_._value
            self._next = next_next
            next_next._prev = self
        return value

    def __iter__(self):
        def iterator(start):
            if self._value is LinkedList.EMPTY:
                return
            yield start._value
            current = start._next
            while current != start:
                yield current._value
                current = current._next
        return iterator(self)
