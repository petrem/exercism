"""Tisbury treasure hunt helpers."""
import operator
from itertools import chain


def get_coordinate(record):
    """Extract coordinates from `record`.

    :param record: tuple - a (treasure, coordinate) pair.
    :return: str - the extracted map coordinate.
    """
    return record[1]


def convert_coordinate(coordinate):
    """Convert string `coordinate` to tuple.

    :param coordinate: str - a string map coordinate
    :return:  tuple - the string coordinate seperated into its individual components.
    """
    return operator.itemgetter(0, 1)(coordinate)


def compare_records(azara_record, rui_record):
    """Compare `azara_record`'s coordinate with that in `rui_record`.

    :param azara_record: tuple - a (treasure, coordinate) pair.
    :param rui_record: tuple - a (location, coordinate, quadrant) trio.
    :return: bool - True if coordinates match, False otherwise.
    """
    return convert_coordinate(get_coordinate(azara_record)) == rui_record[1]


def create_record(azara_record, rui_record):
    """With the powers of azara and rui combined, go Gadget!

    :param azara_record: tuple - a (treasure, coordinate) pair.
    :param rui_record: tuple - a (location, coordinate, quadrant) trio.
    :return:  tuple - combined record, or "not a match" if the records are incompatible.
    """
    if compare_records(azara_record, rui_record):
        return azara_record + rui_record
    else:
        return "not a match"


def clean_up(combined_record_group):
    """Silly thing to do with data.

    :param combined_record_group: tuple of tuples - everything from both participants.
    :return: string of tuples separated by newlines - everything "cleaned".
             Excess coordinates and information removed.
    """
    return _unlines(
        str(operator.itemgetter(0, 2, 3, 4)(record))
        if isinstance(record, tuple)
        else record
        for record in combined_record_group
    )


def _unlines(iterable):
    return "\n".join(chain(iterable, ("",)))
