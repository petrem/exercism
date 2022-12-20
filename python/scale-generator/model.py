import abc
from enum import IntEnum, StrEnum, global_enum
from typing import Iterable, NewType, Self

from modulo import Mod12

__all__ = [
    # types
    "NoteName",
    "Accidental",
    # NoteName letter constants
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    # NoteName solfège constants
    "DO",
    "RE",
    "MI",
    "FA",
    "SOL",
    "SO",
    "LA",
    "SI",
    "TI",
    # Accidental constants
    "NATURAL",
    "SHARP",
    "DOUBLESHARP",
    "FLAT",
    "DOUBLEFLAT",
    # Interval constants
    "P1",
    "m2",
    "M2",
    "A2",
    "m3",
    "M3",
    "P4",
    "d5",
    "P5",
    "m6",
    "M6",
    "m7",
    "M7",
    "P8",
]


@global_enum
class NoteName(StrEnum):

    """Note name enumeration: A, B ... G.

    Also defined are solfège name aliases.

    Can be initialised from a string starting with letters A..G in upper or lower case,
    or a string containing a solfège name.

    Accepted solfège names are: "do", "re", "mi", "fa", "so" or "sol", "la" and
    "si" or "ti".
    """

    A = LA = "A"
    B = SI = TI = "B"
    C = DO = "C"
    D = RE = "D"
    E = MI = "E"
    F = FA = "F"
    G = SOL = SO = "G"

    @classmethod
    def _missing_(cls, value) -> Self | None:
        assert isinstance(value, str)
        value = value.upper()
        for name, member in cls.__members__.items():
            if name == value:
                return member
        return None

    def __ascii__(self) -> str:
        return self.__str__()


# TODO: @global_enum is strange; it places the names in the module's NS,
# but tools (flake8, mypy) won't detect them.
# This is a workaround.
A, LA, B, SI, TI, C, DO, D, RE, E, MI, F, FA, G, SOL, SO = NoteName.__members__.values()


@global_enum
class Accidental(StrEnum):

    """Accidental enumeration: natural, flats and sharps.

    Can be initiliased from a string starting with either the unicode or ascii
    representations: "♮" or "", "♯" or "#", "𝄪" or "##", "♭" or "b" and "𝄫" or "bb".

    """

    NATURAL = ("♮", "")
    SHARP = ("♯", "#")
    DOUBLESHARP = ("𝄪", "##")
    FLAT = ("♭", "b")
    DOUBLEFLAT = ("𝄫", "bb")

    # Fool linters: make them believe we have this attribute. Enum's way of adding
    # attributes in __new__ instead of __self__ is not particularly pleasing.
    _ascii: str
    _ignore_ = ["_ascii"]

    def __new__(cls, unicode_repr: str, ascii_repr: str) -> Self:  # noqa: D102
        obj = str.__new__(cls, unicode_repr)
        obj._value_ = unicode_repr
        obj._ascii = ascii_repr
        return obj

    @classmethod
    def _missing_(cls, value) -> Self:
        assert isinstance(value, str)
        return next((a for a in cls if a.value == value or a._ascii == value), None)

    def __ascii__(self) -> str:
        return self._ascii


NATURAL, SHARP, DOUBLESHARP, FLAT, DOUBLEFLAT = Accidental.__members__.values()


class Note:

    """Represent a particular note name and accidental.

    This is *NOT* about pitch classes and equivalence!
    In particular, enharmonics are not "equal" for this class.
    """

    __slots__ = "name", "accidental"

    def __init__(self, name: NoteName, accidental: Accidental):
        self.name = name
        self.accidental = accidental

    def __eq__(self, other):
        if other.__class__ == self.__class__:
            return self.name == other.name and self.accidental == other.accidental
        return NotImplemented

    def __str__(self):
        return f"{self.name}{self.accidental}"

    def __repr__(self):
        return f"Note<{self.name}{self.accidental}>"

    def __ascii__(self) -> str:
        return f"{self.name.__ascii__()}{self.accidental.__ascii__()}"

    @property
    def pitch_class(self) -> "PitchClass":
        return PitchClass.parse(self)


PitchInterval = NewType("PitchInterval", int)


class PitchClassInterval(Mod12):
    def __init__(self, value):
        Mod12.__init__(int(value))

    def __repr__(self):
        return f"PitchClassInterval({self})"


@global_enum
class Interval(IntEnum):

    """Represent common intervals, in ET semitones."""

    # pylint: disable=invalid-name
    P1 = 0
    m2 = 1
    M2 = 2
    m3 = A2 = 3
    M3 = 4
    P4 = 5
    d5 = 6
    P5 = 7
    m6 = 8
    M6 = 9
    m7 = 10
    M7 = 11
    P8 = 12

    @classmethod
    def _missing_(cls, value) -> Self:
        match value:
            case int():
                return cls(value)
            case str():
                return cls.__members__.get(value)
            case _:
                return None


P1, m2, M2, A2, m3, M3, P4, d5, P5, m6, M6, m7, M7, P8 = Interval.__members__.values()


# TODO: is it useful to have this as an enumeration?
class PitchClass(IntEnum):

    """Represent pitch classes."""

    # workaround for linters: ``Enum`` attributes are set using
    # ``__new__` instead of ``__init__``
    enharmonics: list[Note]
    _ignore_ = ["enharmonics"]

    def __new__(cls, value: int, enharmonics: Iterable[Note]) -> Self:
        obj = int.__new__(cls, value)
        obj._value_ = value
        obj.enharmonics = list(enharmonics)
        return obj

    @classmethod
    def _missing_(cls, value) -> Self:
        assert isinstance(value, int)
        return cls(value % 12)

    @classmethod
    def parse(cls, value: int | str | tuple[NoteName, Accidental] | Note) -> Self:
        """Read pcs in several forms.

        * ``int``s are supported to be versatile, even if we can use ``PitchClass()``
        * ``str``s can be read by lower/upper note name and accidental [not implemented]
        * ``Note``s
        * ``(NoteName, Accidental)`` tuples

        """
        match value:
            case int(x):
                return cls(x % 12)
            case (NoteName() as nn, Accidental() as a):
                return cls._find_note(Note(nn, a))
            case Note() as n:
                return cls._find_note(n)
            case str():
                raise NotImplementedError("Don't know how to read strings yet.")
        return None

    @classmethod
    def _find_note(cls, note: Note) -> Self:
        return next((pc for pc in cls if note in pc.enharmonics), None)

    def transpose(self, pci: PitchClassInterval) -> Self:
        """Transpose upward/clockwise."""
        return self.__class__(pci + self.value)

    def __add__(self, other: PitchClassInterval | Interval | int) -> Self:
        match other:
            case self.__class__():
                pci = other
            case int():
                pci = PitchClassInterval(other)
            case Interval():
                pci = PitchClassInterval(other.value)
            case _:
                raise TypeError(f"Cannot add PitchClass with {type(other)}")
        return self.transpose(pci)

    ZERO = (0, (Note(B, SHARP), Note(C, NATURAL)))
    ONE = (1, (Note(C, SHARP), Note(D, FLAT)))
    TWO = (2, (Note(D, NATURAL),))
    THREE = (3, (Note(D, SHARP), Note(E, FLAT)))
    FOUR = (4, (Note(E, NATURAL), Note(F, FLAT)))
    FIVE = (5, (Note(E, SHARP), Note(F, NATURAL)))
    SIX = (6, (Note(F, SHARP), Note(G, FLAT)))
    SEVEN = (7, (Note(G, NATURAL),))
    EIGHT = (8, (Note(G, SHARP), Note(A, FLAT)))
    NINE = (9, (Note(A, NATURAL),))
    TEN = (10, (Note(A, SHARP), Note(B, FLAT)))
    ELEVEN = (11, (Note(B, NATURAL), Note(C, FLAT)))


class AbstractPC2NoteStrategy(metaclass=abc.ABCMeta):

    """Abstract base class for a strategy of getting notes from PCs."""

    @classmethod
    @abc.abstractmethod
    def get_key(cls, *args, **kwargs):
        """Sort key maximising by the strategy's priority."""
        raise NotImplementedError

    @classmethod
    def get_note(cls, pitch_class: PitchClass, *args, **kwargs) -> Note:
        """Get note from ``pitch_class`` according to this strategy."""
        return max(pitch_class.enharmonics, key=cls.get_key(*args, **kwargs))


class NoteNameStrategy(AbstractPC2NoteStrategy):

    """Get Note with specified name (if possible)."""

    @classmethod
    def get_key(cls, *args, **kwargs):
        note_name: NoteName = args[0]
        return lambda note: 1 if note.name == note_name else 0


class AbstractAccidentalKindStrategy(AbstractPC2NoteStrategy):

    """Get Note based on some rule about accidental."""

    PRIO: dict[Accidental, int] | None = None

    @classmethod
    def get_key(cls, *args, **kwargs):
        return lambda note: cls.PRIO[note.accidental]


class UseNaturalOrSharpsStrategy(AbstractAccidentalKindStrategy):

    """Get natural Note if possible, or else try using sharps first."""

    PRIO = {
        NATURAL: 4,
        SHARP: 3,
        DOUBLESHARP: 2,
        DOUBLEFLAT: 1,
        FLAT: 0,
    }


class UseNaturalOrFlatsStrategy(AbstractAccidentalKindStrategy):

    """Get natural Note if possible, or else try using flats first."""

    PRIO = {
        NATURAL: 4,
        FLAT: 3,
        DOUBLEFLAT: 2,
        DOUBLESHARP: 1,
        SHARP: 0,
    }
