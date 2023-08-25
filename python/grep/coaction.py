"""Coroutines.

Based on original idea of David Beazley, described on
http://dabeaz.com/coroutines/ , bended and twisted to evil.

Goes a step further than just starting a coroutine automatically on call:
- implements the inner loop of the coroutine
- the decorated function must be a contextmanager yielding a computation ("action")
  that takes the data sent into the coroutine
- the result of this computation is sent out to the target

This simplifies the coroutines code, but reduces what they can do.
"""
from inspect import getfullargspec
from functools import wraps


class CoAction:
    def __init__(self, func, target=None):
        self.func = func
        self.target = target

    def __or__(self, other):
        if isinstance(other, CoAction):
            self.target = other
            return self
        raise TypeError(f"Expected CoAction, not {type(other)}")

    def inner_loop(self, *args, **kwargs):
        try:
            while True:
                data = (yield)
                self.func(data)
        except GeneratorExit:
            if self.target is not None:
                self.target.close()


def coaction(varname):
    """Coroutine action decorator."""
    def wrapper(func):
        def start(*args, **kwargs):
            #data_arg = _getarg(func, "target", args, kwargs)
            ca = CoAction(func).inner_loop(*args, **kwargs)
            next(ca)
            return ca
        return start
    return wrapper


def _getarg(func, argname, args, kwargs):
    argspec = getfullargspec(func)
    if argname in argspec.args:
        return args[argspec.args.index(argname)]
    elif argname in argspec.kwonlyargs:
        return kwargs.get(argname, argspec.kwonlydefaults(argname))
    return None


def compose(f, g):
    """Functools extra: compose two functions where the second takes exactly one arg."""
    return lambda *args: f(*args[1:], g(args[0]))

@coaction("foo")
def something(a):
    return foo
