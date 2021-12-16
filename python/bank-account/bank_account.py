from functools import wraps
from threading import Lock


def synchronized(fn):
    @wraps(fn)
    def wrapped(self, *args, **kwargs):
        with self._lock:
            return fn(self, *args, **kwargs)
    return wrapped


class BankAccount(object):
    def __init__(self):
        self.balance = None
        self._lock = Lock()

    def get_balance(self):
        if self.balance is None:
            raise ValueError("Account not open")
        return self.balance

    @synchronized
    def open(self):
        if self.balance is not None:
            raise ValueError("Account already open")
        self.balance = 0

    @synchronized
    def deposit(self, amount):
        if amount < 0:
            raise ValueError("Cannot deposit negative amount. You wanted withdraw?")
        try:
            self.balance += amount
        except TypeError:
            raise ValueError("Account not open")

    @synchronized
    def withdraw(self, amount):
        if amount < 0:
            raise ValueError("Cannot withdraw negative amount. You wanted deposit?")
        try:
            balance = self.balance - amount
            if balance < 0:
                raise ValueError("Sneaky, but no. Try a smaller amount.")
            self.balance = balance
        except TypeError:
            raise ValueError("Account not open")

    @synchronized
    def close(self):
        if self.balance is None:
            raise ValueError("Account not open")
        self.balance = None
