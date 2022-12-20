"""Modular integers."""

from functools import wraps


def modular_int(modulo):  # noqa: C901
    """Class decorator adding modular arithmetic in classes."""

    def wrapper(cls):
        @wraps(cls, updated=())
        class ModularWrapped(cls):

            """Custom ``modulo`` int class."""

            def __new__(cls, value, *args, **kwargs):
                return super().__new__(cls, value % modulo, *args, **kwargs)

            def __add__(self, other):
                if isinstance(other, int):
                    return self.__class__(super().__add__(other) % modulo)
                return NotImplemented

            def __sub__(self, other):
                if isinstance(other, int):
                    return self.__class__(super().__sub__(other) % modulo)
                return NotImplemented

            def __mul__(self, other):
                if isinstance(other, int):
                    return self.__class__(super().__mul__(other) % modulo)
                return NotImplemented

            def __div__(self, other):
                if isinstance(other, int):
                    return self.__class__(super().__div__(other) % modulo)
                return NotImplemented

            def __neg__(self):
                return self.__class__(super().__neg__() % modulo)

            def __str__(self):
                return f"{int(self):d}"

            def __repr__(self):
                return f"{cls.__name__}({self})"

        return ModularWrapped

    return wrapper


@modular_int(12)
class Mod12(int):

    """Modulo 12 integer."""
