from abc import ABC, abstractmethod
import re


TOKEN_LIST = []


def register(order):
    def wrapper(klass):
        TOKEN_LIST.append(klass)
        return klass
    return wrapper


class AbcToken(ABC):
    def __init__(self):
        pass

    @abstractmethod
    def __str__(self):
        raise NotImplementedError

    @abstractmethod
    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False


class _SymbolToken(AbcToken):
    SYMBOL = NotImplemented

    def __init__(self):
        super().__init__()

    def __str__(self):
        return self.SYMBOL

    def __eq__(self):
        return super().__eq__(self, other)

    def match(self, s: str) -> bool:
        return self.SYMBOL == s


class 𝜖Token(_SymbolToken):
    SYMBOL = ""


class LeftRoundBracketToken(_SymbolToken):
    SYMBOL = "("


class RightRoundBracketToken(_SymbolToken):
    SYMBOL = ")"


class LeftSquareBracketToken(_SymbolToken):
    SYMBOL = "["


class RightSquareBracketToken(_SymbolToken):
    SYMBOL = "]"


class ColonToken(_SymbolToken):
    SYMBOL = ":"


class ColonToken(_SymbolToken):
    SYMBOL = ":"


class SemiColonToken(_SymbolToken):
    SYMBOL = ";"


class PlusToken(_SymbolToken):
    SYMBOL = "+"


class MinusToken(_SymbolToken):
    SYMBOL = "-"


class UcLetterToken(AbcToken):
    def __init__(self, letter: str):
        super().__init__()
        if len(letter) != 1 or not letter.isupper():
            raise ValueError(f"Not an uppercase letter: {letter}")
        self.letter = letter

    def __str__(self):
        return self.letter

    def __eq__(self, other):
        return super().__eq__(self, other) and self.letter == other.letter

    def match(self, s: str) -> bool:
        return self.letter == s


class DigitToken(AbcToken):
    def __init__(self, digit: str):
        super().__init__()
        if len(digit) != 1 or not digit.isdigit():
            raise ValueError(f"Not a digit: {digit}")
        self.digit = digit

    def __str__(self):
        return self.digit

    def __eq__(self, other):
        return super().__eq__(self, other) and self.digit == other.digit

    def match(self, s: str) -> bool:
        return self.digit == s


TOKEN_CHECK_ORDER = [UcLetterToken, DigitToken, 𝜖Token]


def skip_space(s: str) -> str:
    m = re.match(r"\s+", s)
    if m:
        return s[m.end():]
    return s


def tokenize(input_string: str):
    remaining = skip_space(input_string)

    while remaining:
        peek = remaining[0]
        if peek


