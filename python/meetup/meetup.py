import calendar
from datetime import date, timedelta
from functools import partialmethod


class MeetupDayException(Exception):
    pass


WEEK = timedelta(days=7)


def meetup(year, month, which, day_of_the_week):
    date_finder = DateFinder(which)
    return date_finder(year, month, day_of_the_week)


class DateFinder:
    def __init__(self, which):
        self.strategy = getattr(self, which)  # n.b. this is fragile

    def __call__(self, year, month, day_of_week):
        return self.strategy(getattr(calendar, day_of_week.upper()), year, month)

    def teenth(self, day_of_week, year, month):
        return self.days_until(date(year, month, 13), day_of_week)

    def last(self, day_of_week, year, month):
        d = self.days_until(date(year, month, 1), day_of_week) + 4 * WEEK
        return d if d.month == month else d - WEEK

    def nth(self, nth, day_of_week, year, month):
        d = self.days_until(date(year, month, 1), day_of_week) + nth * WEEK
        if d.month != month:
            raise MeetupDayException("That day does not exist.")
        return d

    first = partialmethod(nth, 0)
    second = partialmethod(nth, 1)
    third = partialmethod(nth, 2)
    fourth = partialmethod(nth, 3)
    fifth = partialmethod(nth, 4)

    @staticmethod
    def days_until(from_date, day_of_week):
        return from_date + timedelta(days=(7 + day_of_week - from_date.weekday()) % 7)
