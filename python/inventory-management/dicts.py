"""Exercism: inventory management."""
from collections import Counter


def create_inventory(items):
    """I will not buy this record, it is scratched.

    :param items: list - list of items to create an inventory from.
    :return:  dict - the inventory dictionary.
    """
    return dict(Counter(items))


def add_items(inventory, items):
    """I will not buy this record, it is scratched.

    :param inventory: dict - dictionary of existing inventory.
    :param items: list - list of items to update the inventory with.
    :return:  dict - the inventory dictionary update with the new items.
    """
    c = Counter(inventory)
    c.update(items)
    return c


def decrement_items(inventory, items):
    """I will not buy this record, it is scratched.

    :PARAM inventory: dict - inventory dictionary.
    :param items: list - list of items to decrement from the inventory.
    :return:  dict - updated inventory dictionary with items decremented.
    """
    c = Counter(inventory)
    c.subtract(items)
    return {k: max(v, 0) for k, v in c.items()}


def remove_item(inventory, item):
    """I will not buy this record, it is scratched.

    :param inventory: dict - inventory dictionary.
    :param item: str - item to remove from the inventory.
    :return:  dict - updated inventory dictionary with item removed.
    """
    inventory.pop(item, None)
    return inventory


def list_inventory(inventory):
    """I will not buy this record, it is scratched.

    :param inventory: dict - an inventory dictionary.
    :return: list of tuples - list of key, value pairs from the inventory dictionary.
    """
    return list(filter(_positive_inventory, inventory.items()))


def _positive_inventory(item):
    return item[1] > 0
