from itertools import cycle
import secrets
import string


class Cipher(object):

    KEYLEN = 100
    ALPHABET = string.ascii_lowercase.encode()
    LEN = len(ALPHABET)
    DELTA = ALPHABET[0]

    def _add(self, x, y):
        return ((x + y - 2 * self.DELTA) % self.LEN) + self.DELTA

    def _sub(self, x, y):
        return ((x - y) % self.LEN) + self.DELTA

    def __init__(self, key=None):
        if key is None:
            self._key = bytes(secrets.choice(self.ALPHABET) for _ in range(self.KEYLEN))
        else:
            self._key = key.encode()

    @property
    def key(self):
        return self._key.decode()

    def encode(self, text):
        return bytes(map(self._add, text.encode(), cycle(self._key))).decode()

    def decode(self, text):
        return bytes(map(self._sub, text.encode(), cycle(self._key))).decode()
