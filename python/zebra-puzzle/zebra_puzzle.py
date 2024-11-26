from collections import namedtuple
from enum import IntEnum, StrEnum
from itertools import permutations


def drinks_water():
    return _find_house(solution(), Drink.WATER).nationality


def owns_zebra():
    return _find_house(solution(), Pet.ZEBRA).nationality


class Solution:
    def __init__(self):
        self.cached = None

    def get(self):
        if self.cached is None:
            self.cached = generate()
        return self.cached


solution = Solution().get


def generate():
    return next(
        _make_houses(colors, nats, pets, drinks, hobbies)
        for nats in permutations(Nationality)
        if _check_rule_10(nats)
        for colors in permutations(Color)
        if _check_rule_6(colors) and _check_rule_2(colors, nats) and _check_rule_15(colors, nats)
        for drinks in permutations(Drink)
        if _check_rule_9(drinks) and _check_rule_5(nats, drinks) and _check_rule_4(colors, drinks)
        for hobbies in permutations(Hobby)
        if _check_rule_8(colors, hobbies) and _check_rule_13(drinks, hobbies) and _check_rule_14(nats, hobbies)
        for pets in permutations(Pet)
        if _check_rule_3(nats, pets) and _check_rule_7(pets, hobbies) and _check_rule_11(pets, hobbies) and _check_rule_12(pets, hobbies)
    )


House = namedtuple("House", "color, nationality, pet, drink, hobby")


def _make_houses(colors, nats, pets, drinks, hobbies):
    return [House(*attrs) for attrs in zip(colors, nats, pets, drinks, hobbies)]


def _find_house(houses, attr):
    return next(
        house for house in houses
        if getattr(house, attr.__class__.__name__.lower()) == attr.value)


class Color(IntEnum):
    RED = 0
    GREEN = 1
    IVORY = 2
    YELLOW = 3
    BLUE = 4


class Nationality(StrEnum):
    ENG = "Englishman"
    SPA = "Spaniard"
    UKR = "Ukranian"
    NOR = "Norwegian"
    JAP = "Japanese"


class Pet(IntEnum):
    DOG = 0
    SNAIL = 1
    FOX = 2
    HORSE = 3
    ZEBRA = 4


class Drink(IntEnum):
    COFFEE = 0
    TEA = 1
    MILK = 2
    JUICE = 3
    WATER = 4


class Hobby(IntEnum):
    DANCING = 0
    PAINTER = 1
    READING = 2
    CHESS = 3
    FOOTBALL = 4


# Rule 1: There are five houses.
# We construct the solution this way.


def _check_rule_2(colors, nats):
    """The Englishman lives in the red house."""
    return all((nats[i] == Nationality.ENG) == (colors[i] == Color.RED)
               for i in range(5))


def _check_rule_3(nats, pets):
    """The Spaniard owns the dog."""
    return all((nats[i] == Nationality.SPA) == (pets[i] == Pet.DOG)
               for i in range(5))


def _check_rule_4(colors, drinks):
    """The person in the green house drinks coffee."""
    return all((colors[i] == Color.GREEN) == (drinks[i] == Drink.COFFEE)
               for i in range(5))


def _check_rule_5(nats, drinks):
    """The Ukrainian drinks tea."""
    return all((nats[i] == Nationality.UKR) == (drinks[i] == Drink.TEA)
               for i in range(5))


def _check_rule_6(colors):
    """The green house is immediately to the right of the ivory house."""
    return (i:=colors.index(Color.GREEN)) > 0 and colors[i-1] == Color.IVORY


def _check_rule_7(pets, hobbies):
    """The snail owner likes to go dancing."""
    return all((pets[i] == Pet.SNAIL) == (hobbies[i] == Hobby.DANCING)
               for i in range(5))


def _check_rule_8(colors, hobbies):
    """The person in the yellow house is a painter."""
    return all((colors[i] == Color.YELLOW) == (hobbies[i] == Hobby.PAINTER)
               for i in range(5))


def _check_rule_9(drinks):
    """The person in the middle house drinks milk."""
    return drinks[2] == Drink.MILK


def _check_rule_10(nats):
    """The Norwegian lives in the first house."""
    return nats[0] == Nationality.NOR


def _check_rule_11(pets, hobbies):
    """The person who enjoys reading lives in the house next to the person with the fox."""
    return abs(hobbies.index(Hobby.READING) - pets.index(Pet.FOX)) == 1


def _check_rule_12(pets, hobbies):
    """The painter's house is next to the house with the horse."""
    return abs(hobbies.index(Hobby.PAINTER) - pets.index(Pet.HORSE)) == 1


def _check_rule_13(drinks, hobbies):
    """The person who plays football drinks orange juice."""
    return drinks[hobbies.index(Hobby.FOOTBALL)] == Drink.JUICE


def _check_rule_14(nats, hobbies):
    """The Japanese person plays chess."""
    return hobbies[nats.index(Nationality.JAP)] == Hobby.CHESS


def _check_rule_15(colors, nats):
    """The Norwegian lives next to the blue house."""
    return abs(nats.index(Nationality.NOR) - colors.index(Color.BLUE)) == 1
