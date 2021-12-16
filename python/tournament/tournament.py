from collections import Counter, defaultdict, namedtuple
from operator import itemgetter


def tally(rows):
    tallied_rows = sum(_parse_rows(rows), _AdditiveDict(neutral_element=Counter))
    results = sorted(
        (
            defaultdict(int, counts, Team=team)
            for team, counts in sorted(tallied_rows.items(), key=itemgetter(0))
        ),
        key=itemgetter("P"),
        reverse=True
    )

    return [
        _make_header(),
        *(_make_row(result) for result in results)
    ]


_WINNER = Counter({"MP": 1, "W": 1, "D": 0, "L": 0, "P": 3})
_LOSER = Counter({"MP": 1, "W": 0, "D": 0, "L": 1, "P": 0})
_DRAW = Counter({"MP": 1, "W": 0, "D": 1, "L": 0, "P": 1})


def _parse_rows(rows):
    for row in rows:
        team_A, team_B, result = row.split(";")
        if result == "win":
            yield {team_A: _WINNER}
            yield {team_B: _LOSER}
        elif result == "loss":
            yield {team_A: _LOSER}
            yield {team_B: _WINNER}
        else:
            yield {team_A: _DRAW}
            yield {team_B: _DRAW}


class _AdditiveDict(dict):
    """A dictionary that supporting addition.

    It is supposed to work similar to a monoid, and you should provide a
    neutral element (or identity) if the values are non-numeric (when the default,
    0, should work).

    The neutral_element can be a callable, useful for mutables."""
    def __init__(self, *args, neutral_element=0, **kwargs):
        super().__init__(*args, **kwargs)
        self._neutral = neutral_element
        if callable(neutral_element):
            self._get_neutral = neutral_element
        else:
            self._get_neutral = lambda: neutral_element

    def __add__(self, other):
        d = dict(self)
        for k, v in other.items():
            d[k] = d.get(k, self._get_neutral()) + v
        return _AdditiveDict(d, neutral_element=self._neutral)


_Col = namedtuple("_Col", "width,align", defaults=(3, ">"))
_COLS = {
    "Team": _Col(30, "<"),
    **{col: _Col() for col in ["MP", "W", "D", "L", "P"]}
}


def _make_header():
    return _make_row({col: col for col in _COLS})


def _make_row(values):
    return " |".join(
        f"{values[col_name]:{col_settings.align}{col_settings.width}}"
        for col_name, col_settings in _COLS.items()
    )
