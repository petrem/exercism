class BufferFullException(Exception):
    pass


class BufferEmptyException(Exception):
    pass


class CircularBuffer:
    def __init__(self, capacity):
        self._buffer = [None] * capacity
        self._capacity = capacity
        self._size = 0
        self._head = 0

    def read(self):
        if self._size <= 0:
            raise BufferEmptyException("Read on empty buffer")
        tail = (self._head + self._capacity - self._size) % self._capacity
        data = self._buffer[tail]
        self._size -= 1
        return data

    def write(self, data):
        if self._size >= self._capacity:
            raise BufferFullException("Write on full buffer")
        self.overwrite(data)

    def overwrite(self, data):
        self._buffer[self._head] = data
        self._head = (self._head + 1) % self._capacity
        self._size = min(self._capacity, self._size + 1)

    def clear(self):
        self._size = 0
