"""Phone Number exercise.

As it is often the case, this solution is gimmicky.

The previous version of the exercise did not require explicit error messages. Now, even
though the regex can detect correct numbers, it can't be used by itself as we won't know
the cause for an error.

For a lark, I'm keeping the regex to detect correct numbers, then pass incorrect ones to
a method that checks for particular problems. For the same lark, I'm playing with
ExceptionGroup that I have never used before and make a questionable use of the fact
``Exception``s are normal classes.

The approach is not great in terms of performance and has many loose ends. We leave it
as an exercise to the reader to enumerate the problems ;-).

"""

import re
import string
from abc import ABCMeta, abstractmethod
from functools import wraps
from typing import Any, ClassVar, Self


class PhoneNumber:  # pylint: disable=no-member
    """North American Numbering Plan Phone Number."""

    CTRY = r"(?:\+?1)?"
    AREA = r"\(?(?P<area_code>[2-9]\d\d)\)?"
    EXCH = r"(?P<exchange>[2-9]\d\d|\([2-9]\d\d\))"
    SUBS = r"(?P<subscriber>\d{4})"
    PHONE_NUMBER = r"(?:\s|[.-])*".join((CTRY, AREA, EXCH, SUBS)) + r"\s*"

    def __init__(self, phone_number: str):
        if not (m := self.parse(phone_number)):
            # pylint: disable=unsubscriptable-object
            err = PhoneNumberError.collect(phone_number).exceptions[0]
            # because for some reasons tests don't allow subclasses
            raise ValueError(str(err))
        for name, value in m.groupdict().items():
            setattr(self, name, value)

    def __getattr__(self, name: str) -> Any:
        """This is for typechecking to pass. Alas."""

        return getattr(self, name)

    @classmethod
    def parse(cls, phone_number: str) -> re.Match | None:
        """Parse a phone number string."""

        return re.fullmatch(cls.PHONE_NUMBER, phone_number)

    @property
    def number(self):
        """Plain number."""

        return f"{self.area_code}{self.exchange}{self.subscriber}"

    def pretty(self) -> str:
        """Formatted number."""

        return f"({self.area_code})-{self.exchange}-{self.subscriber}"


class PhoneNumberError(ValueError, metaclass=ABCMeta):
    """Base exception for phone_number."""

    __ERRORS: ClassVar[list[Self]] = []

    def __init__(self):
        super().__init__(self.__doc__)

    @classmethod
    @abstractmethod
    def check(cls, number: str) -> bool:
        """Run a check on ``number``."""

        raise NotImplementedError

    @classmethod
    def register(cls, err):
        """Decorator to collect defined errors into a list."""

        @wraps(err)
        def wrapper(*args, **kwargs):
            return err(*args, **kwargs)

        cls.__ERRORS.append(err())
        return wrapper

    @classmethod
    def collect(cls, number: str) -> ExceptionGroup:
        """Collect all registered errors for ``number``."""

        return ExceptionGroup(
            f"Issues for {number}",
            [err for err in cls.__ERRORS if err.check(number)]
            or [PhoneNumberUnknownError],
        )


# Order is important (and discipline... what?!).


@PhoneNumberError.register
class PhoneNumberContainsPunctuationError(PhoneNumberError):
    """punctuations not permitted"""

    INVALID_CHARS = frozenset(string.punctuation) - set("+-().")

    @classmethod
    def check(cls, number: str) -> bool:
        # for short strings, this seems to be faster than ``any(...)``
        for char in cls.INVALID_CHARS:  # noqa: SIM110
            if char in number:
                return True
        return False


@PhoneNumberError.register
class PhoneNumberContainsLettersError(PhoneNumberError):
    """letters not permitted"""

    @classmethod
    def check(cls, number) -> bool:
        for char in string.ascii_letters:  # noqa: SIM110
            if char in number:
                return True
        return False


@PhoneNumberError.register
class PhoneNumberTooShortError(PhoneNumberError):
    """must not be fewer than 10 digits"""

    @classmethod
    def check(cls, number: str) -> bool:
        return len(_to_digits(number)) < 10


@PhoneNumberError.register
class PhoneNumberTooLongError(PhoneNumberError):
    """must not be greater than 11 digits"""

    @classmethod
    def check(cls, number: str) -> bool:
        return len(_to_digits(number)) > 11


@PhoneNumberError.register
class PhoneNumberFirstDigitNotOneError(PhoneNumberError):
    """11 digits must start with 1"""

    @classmethod
    def check(cls, number: str) -> bool:
        digits = _to_digits(number)
        return len(digits) == 11 and digits[0] != "1"


@PhoneNumberError.register
class PhoneNumberAreaStartsWithZeroError(PhoneNumberError):
    """area code cannot start with zero"""

    @classmethod
    def check(cls, number: str) -> bool:
        # This and most following checks will crash if not called in the right order.
        # Meh.
        return (digits := _to_digits(number))[len(digits) - 10] == "0"


@PhoneNumberError.register
class PhoneNumberAreaStartsWithOneError(PhoneNumberError):
    """area code cannot start with one"""

    @classmethod
    def check(cls, number: str) -> bool:
        return (digits := _to_digits(number))[len(digits) - 10] == "1"


@PhoneNumberError.register
class PhoneNumberExchangeStartsWithZeroError(PhoneNumberError):
    """exchange code cannot start with zero"""

    @classmethod
    def check(cls, number: str) -> bool:
        return (digits := _to_digits(number))[len(digits) - 7] == "0"


@PhoneNumberError.register
class PhoneNumberExchangeStartsWithOneError(PhoneNumberError):
    """exchange code cannot start with one"""

    @classmethod
    def check(cls, number: str) -> bool:
        return (digits := _to_digits(number))[len(digits) - 7] == "1"


# deliberately not registered
class PhoneNumberUnknownError(PhoneNumberError):
    """unknown error"""

    @classmethod
    def check(cls, number: str) -> bool:
        return True


def _to_digits(a_string):
    return "".join(c for c in a_string if c.isdecimal() and c.isascii())
