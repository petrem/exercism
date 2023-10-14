"""Exercise: circular-buffer"""


class BufferFullException(BufferError):
    """The buffer is full."""


class BufferEmptyException(BufferError):
    """The buffer is empty."""


class CircularBuffer:
    """A circular buffer of fixed size."""

    def __init__(self, capacity):
        self._buffer = [None] * capacity
        self._capacity = capacity
        self._size = 0
        self._head = 0

    def read(self):
        """Read oldest data from the buffer."""
        if self._size <= 0:
            raise BufferEmptyException("Circular buffer is empty")
        tail = (self._head + self._capacity - self._size) % self._capacity
        self._size -= 1
        return self._buffer[tail]

    def write(self, data):
        """Write data into the buffer, if not full."""
        if self._size >= self._capacity:
            raise BufferFullException("Circular buffer is full")
        self.overwrite(data)

    def overwrite(self, data):
        """Write data into the buffer, overwriting oldest data if full."""
        self._buffer[self._head] = data
        self._head = (self._head + 1) % self._capacity
        self._size = min(self._capacity, self._size + 1)

    def clear(self):
        """Truncate the buffer."""
        self._size = 0
