import re
from collections import namedtuple
from enum import Flag, auto
from functools import partial, reduce, wraps


def parse(markdown):
    line_parser = compose(
        _parse_paragraph,
        _parse_italic,
        _parse_bold,
        _parse_heading,
        _parse_list_item
    )
    return "".join(value for value in _parse(markdown, line_parser))


def _parse(markdown, line_parser):
    in_list = False
    for line in markdown.splitlines():
        parsed_line = line_parser(ParserText(line))
        is_li = ParserAttrs.LIST_ITEM in parsed_line.attrs
        if is_li and not in_list:
            yield "<ul>"
            in_list = True
        elif not is_li and in_list:
            yield "</ul>"
            in_list = False
        yield parsed_line.text
    if in_list:
        yield "</ul>"


# Alternative
def parse2(markdown):
    parsed_results = []
    line_parser = compose(
        _parse_paragraph,
        _parse_italic,
        _parse_bold,
        _parse_heading,
        _parse_list_item
    )
    outside = outside_list(line_parser)
    next(outside)
    for line in markdown.splitlines():
        parsed_line = outside.send(line)
        parsed_results.append(parsed_line)
    parsed_results.append(outside.send(None))
    return "".join(parsed_results)


def outside_list(line_parser):
    line = yield
    while True:
        if line is None:
            yield ""
        parsed_line = line_parser(ParserText(line))
        if ParserAttrs.LIST_ITEM in parsed_line.attrs:
            last_in_list = yield from inside_list(line_parser, parsed_line.text)
            parsed_line = ParserText(last_in_list)
        line = yield parsed_line.text


def inside_list(line_parser, starter_line):
    line = yield f"<ul>{starter_line}"
    while True:
        if line is None:
            return "</ul2>"
        parsed_line = line_parser(ParserText(line))
        if ParserAttrs.LIST_ITEM not in parsed_line.attrs:
            return f"</ul>{parsed_line.text}"
        line = yield parsed_line.text


# End of alternative


# Line parsing

class ParserAttrs(Flag):
    NONE = 0
    LIST_ITEM = auto()
    HAS_CONTAINER = auto()


ParserText = namedtuple("ParserText", "text,attrs", defaults=[ParserAttrs.NONE])


def parser(regex=None):
    regex = re.compile(regex) if regex else None

    def outer(fn):
        @wraps(fn)
        def inner(src, match=None):
            if regex:
                m = re.match(regex, src.text)
                if m:
                    result = fn(src, m)
                else:
                    result = src
            else:
                result = fn(src)
            return ParserText(result.text, result.attrs | src.attrs)
        return inner
    return outer


@parser()
def _parse_paragraph(parsertxt):
    if ParserAttrs.HAS_CONTAINER not in parsertxt.attrs:
        return ParserText(_p(parsertxt.text), ParserAttrs.HAS_CONTAINER)
    else:
        return parsertxt


@parser(r"(?P<hashes>#|##|######) (?P<title>.*)")
def _parse_heading(parsertxt, match=None):
    return ParserText(
        _heading(len(match["hashes"]), match["title"]),
        ParserAttrs.HAS_CONTAINER
    )


@parser(r"\* (?P<item>.*)")
def _parse_list_item(parsertxt, match=None):
    return ParserText(
        _li(match["item"]),
        ParserAttrs.HAS_CONTAINER | ParserAttrs.LIST_ITEM
    )


@parser(r"(.*)__(.*)__(.*)")
def _parse_bold(parsertxt, match=None):
    return ParserText(_bold(*match.groups()))


@parser(r"(.*)_(.*)_(.*)")
def _parse_italic(line, match=None):
    return ParserText(_italic(*match.groups()))


# Compose

def compose(*functions):
    """Compose given functions of one (mandatory) argument.

    Returns a callable expecting one argument.
    """
    return reduce(lambda f, g: lambda x: f(g(x)), functions)


# HTML tag helpers

def _tagged(tag, text):
    return f"<{tag}>{text}</{tag}>"


def _tag_and_itercalate(tag, before, middle, after):
    return before + _tagged(tag, middle) + after


def _heading(level, text):
    return _tagged(f"h{level}", text)


_bold = partial(_tag_and_itercalate, "strong")
_italic = partial(_tag_and_itercalate, "em")
_li = partial(_tagged, "li")
_p = partial(_tagged, "p")
