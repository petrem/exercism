from operator import add
from functools import reduce


def compose2(fn1, fn2):
    return lambda *args: fn1(fn2(*args))


def flip(fn):
    return lambda a, b: fn(b, a)


append = add
filter = compose2(list, __builtins__["filter"])
length = len
map = compose2(list, __builtins__["map"])
foldl = reduce
reverse = compose2(list, reversed)


def concat(lists):
    return reduce(add, lists, [])


def foldr(f, list_, initial):
    return reduce(flip(f), reversed(list_), initial)
