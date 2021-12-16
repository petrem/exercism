import calendar
from datetime import date, timedelta
from functools import partial


class MeetupDayException(Exception):
    pass


WEEK = timedelta(days=7)


def _magic(base, skip, day_of_the_week):
    daydiff = (7 + day_of_the_week - base.weekday()) % 7
    return base + skip * WEEK + timedelta(days=daydiff)


def _teenth(day_of_the_week, month1st):
    return _magic(month1st + timedelta(days=12), 0, day_of_the_week)


def _last(day_of_the_week, month1st):
    d = _magic(month1st, 4, day_of_the_week)
    if d.month == month1st.month:
        return d
    else:
        return d - WEEK


def _nth(nth, day_of_the_week, month1st):
    d = _magic(month1st, nth, day_of_the_week)
    if d.month != month1st.month:
        raise MeetupDayException(
            f"No {nth}th d.strftime('%A') in {month1st.strftime('%Y-%m')}")
    return d


STRATEGIES = {
    "teenth": _teenth,
    "last": _last,
    "1st": partial(_nth, 0),
    "2nd": partial(_nth, 1),
    "3rd": partial(_nth, 2),
    "4th": partial(_nth, 3),
    "5th": partial(_nth, 4),
}


def meetup_day(year, month, day_of_the_week, which):
    month1st = date(year, month, 1)
    return STRATEGIES[which](
        getattr(calendar, day_of_the_week.upper()),  # n.b.: dangerous
        month1st
    )
