import re


class Phone(object):

    RE_CTRY = r"(?P<country>\+?1)?"
    RE_AREA = r"\(?(?P<area>[2-9]\d\d)\)?"
    RE_EXCH = r"(?P<exchange>[2-9]\d\d|\([2-9]\d\d\))"
    RE_SUBS = r"(?P<subscriber>\d{4})"
    RE_PHONE_NUMBER = r"(?:\s*|[.-])?".join((RE_CTRY, RE_AREA, RE_EXCH, RE_SUBS)) + r"\s*"

    def __init__(self, phone_number):
        m = re.fullmatch(self.RE_PHONE_NUMBER, phone_number)
        if not m:
            raise ValueError(f"Could not parse {phone_number}")
        self._country = m.group("country")
        self._area = m.group("area")
        self._exchange = m.group("exchange")
        self._subscriber = m.group("subscriber")

    @property
    def area_code(self):
        return self._area

    @property
    def number(self):
        return "".join((self._area, self._exchange, self._subscriber))

    def pretty(self):
        return f"({self._area}) {self._exchange}-{self._subscriber}"
