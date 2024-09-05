"""Phone Number exercise.

As it is often the case, this solution is gimmicky. The regex should be fair, though.
"""

import re


class PhoneNumber:  # pylint: disable=no-member
    """NANP Phone Number."""

    CTRY = r"(?:\+?1)?"
    AREA = r"\(?(?P<area_code>[2-9]\d\d)\)?"
    EXCH = r"(?P<exchange>[2-9]\d\d|\([2-9]\d\d\))"
    SUBS = r"(?P<subscriber>\d{4})"
    PHONE_NUMBER = r"(?:\s*|[.-])?".join((CTRY, AREA, EXCH, SUBS)) + r"\s*"

    def __init__(self, phone_number):
        if not (m := re.fullmatch(self.PHONE_NUMBER, phone_number)):
            raise ValueError(f"Could not parse {phone_number}")
        for name, value in m.groupdict().items():
            setattr(self, name, value)

    @property
    def number(self):
        """Plain number."""
        return f"{self.area_code}{self.exchange}{self.subscriber}"

    def pretty(self):
        """Formatted number."""
        return f"({self.area_code})-{self.exchange}-{self.subscriber}"
