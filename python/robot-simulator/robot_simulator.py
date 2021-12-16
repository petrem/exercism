from enum import Enum
from functools import partialmethod, wraps
from itertools import starmap
from operator import add, methodcaller


class Bearing(Enum):
    NORTH = 0
    EAST = 1
    SOUTH = 2
    WEST = 3

    def __add__(self, other):
        if isinstance(other, int):
            return Bearing((self.value + other) % 4)
        return NotImplemented


# for tests
NORTH, EAST, SOUTH, WEST = Bearing


class Vector:
    def __init__(self, *values):
        self._vector = tuple(values)

    def __add__(self, other):
        if isinstance(other, Vector):
            return Vector(*starmap(add, zip(self._vector, other._vector)))
        return NotImplemented

    def as_tuple(self):
        return self._vector


DISPLACEMENTS = {
    Bearing.NORTH: Vector(0, 1),
    Bearing.EAST: Vector(1, 0),
    Bearing.SOUTH: Vector(0, -1),
    Bearing.WEST: Vector(-1, 0),
}


def actions(cls):
    cls.ACTIONS = {
        obj.action_code: methodcaller(attr)
        for attr, obj in cls.__dict__.items()
        if callable(obj) and hasattr(obj, "action_code")
    }
    return cls


def action(code):
    def decorator(fn):
        @wraps(fn)
        def wrapper(*args, **kwargs):
            return fn(*args, **kwargs)

        wrapper.action_code = code
        return wrapper
    return decorator


@actions
class Robot:
    def __init__(self, direction=Bearing.NORTH, x=0, y=0):
        self.direction = direction
        self._coordinates = Vector(x, y)

    @property
    def coordinates(self):
        return self._coordinates.as_tuple()

    def move(self, instructions):
        for instruction in instructions:
            Robot.ACTIONS[instruction](self)

    @action("L")
    def turn_left(self):
        self.direction += -1

    @action("R")
    def turn_right(self):
        self.direction += 1

    @action("A")
    def advance(self):
        self._coordinates += DISPLACEMENTS[self.direction]
