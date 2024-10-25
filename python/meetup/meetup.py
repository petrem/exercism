import calendar
from datetime import date
from functools import partialmethod


class MeetupDayException(Exception):
    pass


def meetup(year, month, which, day_of_the_week):
    date_finder = DateFinder(which)
    return date_finder(year, month, day_of_the_week)


class DateFinder:
    def __init__(self, which):
        self.get_base = getattr(self, which)  # n.b. this is fragile

    def __call__(self, year, month, day_of_week):
        day_of_week = getattr(calendar, day_of_week.upper())
        base = self.get_base(year, month)
        try:
            base_weekday = date(year, month, base).weekday()
            return date(year, month, base + (7 + day_of_week - base_weekday) % 7)
        except ValueError as e:
            raise MeetupDayException("That day does not exist.") from e

    def teenth(self, _year, _month):
        return 13

    def last(self, year, month):
        return calendar.monthrange(year, month)[1] - 6

    def nth(self, nth, _year, _month):
        return nth * 7 + 1

    first = partialmethod(nth, 0)
    second = partialmethod(nth, 1)
    third = partialmethod(nth, 2)
    fourth = partialmethod(nth, 3)
    fifth = partialmethod(nth, 4)
