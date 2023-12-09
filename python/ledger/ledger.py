"""Exercise: ledger."""
from dataclasses import InitVar, dataclass, field
from datetime import datetime
from operator import attrgetter


@dataclass
class create_entry:
    """Create ledger entry."""

    datestr: InitVar[str]
    description: str
    change: float | int
    date: datetime = field(init=False)

    def __post_init__(self, datestr):
        self.date = datetime.strptime(datestr, "%Y-%m-%d")


def format_entries(currency, locale, entries):
    """Format ledger entries into a table according to locale."""
    sorted_entries = sorted(entries, key=attrgetter("date", "change", "description"))
    return "\n".join(_generate_rows(currency, locale, sorted_entries))


_LOCALES = {
    "en_US": {
        "headers": ["Date", "Description", "Change"],
        "date_fmt": "%m/%d/%Y",
        "group_sep": ",",
        "decimal_sep": ".",
        "cur_fmt": "{sym}{value} ",
        "cur_neg_fmt": "({sym}{value})",
    },
    "nl_NL": {
        "headers": ["Datum", "Omschrijving", "Verandering"],
        "date_fmt": "%d-%m-%Y",
        "group_sep": ".",
        "decimal_sep": ",",
        "cur_fmt": "{sym} {value} ",
        "cur_neg_fmt": "{sym} -{value} ",
    },
}

_CURRENCY_SYM = {
    "USD": "$",
    "EUR": "€",
}


def _col(width, trunc=None, align="left"):
    _trunc = trunc or (lambda x, _: x)
    align = "" if align == "left" else ">"

    def _format(col):
        return f"{_trunc(col, width):{align}{width}}"

    return _format


def _dots_truncate(s, width):
    return s[: width - 3] + "..." if len(s) > width else s


_HEADER_FORMATTERS = (_col(10), _col(25), _col(13))
_COL_FORMATTERS = (_col(10), _col(25, _dots_truncate), _col(13, align="right"))


def _generate_rows(currency, locale, entries):
    lc_defs = _LOCALES[locale]
    yield _format_row(lc_defs["headers"], formatters=_HEADER_FORMATTERS)
    for entry in entries:
        yield _format_row(
            (
                entry.date.strftime(lc_defs["date_fmt"]),
                entry.description,
                _format_currency(entry.change, currency, lc_defs),
            )
        )


def _format_currency(change, currency, lc_defs):
    change_fmt = "cur_neg_fmt" if change < 0 else "cur_fmt"
    value = f"{abs(change / 100):_.2f}".replace(".", lc_defs["decimal_sep"]).replace(
        "_", lc_defs["group_sep"]
    )
    return lc_defs[change_fmt].format(sym=_CURRENCY_SYM[currency], value=value)


def _format_row(cols, formatters=_COL_FORMATTERS):
    return " | ".join(f(c) for c, f in zip(cols, formatters, strict=True))
