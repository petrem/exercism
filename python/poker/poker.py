"""For all those inclined to point out simpler solutions: GO AWAY, I'll PASS! ;-)

This is a play on python possibilities.

"""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass, field
from enum import Enum, IntEnum, auto
from functools import total_ordering, wraps
from operator import itemgetter, lt
from typing import Callable, Iterable, List


def best_hands(hands: str) -> List[str]:
    return [str(best) for best in maximums(Hand(hand) for hand in hands)]


def maximums(iterable: Iterable) -> list:
    maxes = []
    iterator = iter(iterable)
    try:
        maxes.append(next(iterator))
    except StopIteration:
        return []
    for value in iterator:
        if value > maxes[0]:
            maxes[:] = [value]
        elif value == maxes[0]:
            maxes.append(value)
    return maxes


class Suit(Enum):
    HEARTS = auto()
    TILES = auto()
    DIAMONDS = TILES
    CLOVERS = auto()
    CLUBS = CLOVERS
    PIKES = auto()
    SPADES = PIKES

    @classmethod
    def read(cls, suit: str) -> Suit:
        try:
            return {
                "H": cls.HEARTS,
                "D": cls.DIAMONDS,
                "S": cls.SPADES,
                "C": cls.CLOVERS,
            }[suit.upper()]
        except KeyError:
            raise ValueError(f"Don't know how to associate {suit} to a suit")

    def __str__(self) -> str:
        return {
            self.HEARTS: "H",
            self.DIAMONDS: "D",
            self.SPADES: "S",
            self.CLOVERS: "C",
        }[self]


class CardRank(int):
    def __new__(cls, value: str) -> CardRank:
        return super().__new__(
            cls,
            {"J": 11, "Q": 12, "K": 13, "A": 14}.get(value.upper(), value)
        )

    def __init__(self, value: str, *args, **kwargs) -> None:
        if self > 14 or self < 2:
            raise ValueError(f"Bad card rank: {value}")

    def __str__(self) -> str:
        if self <= 10:
            return super().__str__()
        else:
            return {11: "J", 12: "Q", 13: "K", 14: "A"}[self]


@dataclass(init=False, order=True, frozen=True)
class Card:
    rank: CardRank
    suit: Suit = field(compare=False)

    def __init__(self, card: str) -> None:
        object.__setattr__(self, "rank", CardRank(card[:-1]))
        object.__setattr__(self, "suit", Suit.read(card[-1]))

    def __str__(self) -> str:
        return str(self.rank) + str(self.suit)


class HandCategory(IntEnum):
    HIGH_CARD = auto()
    ONE_PAIR = auto()
    TWO_PAIR = auto()
    THREE_OF_A_KIND = auto()
    STRAIGHT = auto()
    FLUSH = auto()
    FULL_HOUSE = auto()
    FOUR_OF_A_KIND = auto()
    STRAIGHT_FLUSH = auto()


# Ace (== 14) can also start a 5-high straight
_SMALL_STRAIGHT = (14, 5, 4, 3, 2)


def _straight_comparator(discriminator1, discriminator2):
    small1 = discriminator1 == _SMALL_STRAIGHT
    small2 = discriminator2 == _SMALL_STRAIGHT
    if small1 or small2:
        return small1 and not small2
    return discriminator1 < discriminator2


def _categorizer(category, compare_function=lt):
    def decorate(fn):
        fn.category = category
        fn.compare_function = compare_function

        @wraps(fn)
        def wrapper(*args, **kwargs):
            return fn(*args, **kwargs)

        return wrapper
    return decorate


@total_ordering
class Hand:
    def __init__(self, hand: str) -> None:
        self._cards = tuple(Card(card) for card in hand.split())
        if len(self._cards) != 5:
            raise ValueError(f"Hand must have exactly 5 cards, got {hand}")
        self._card_counts = Counter(self._cards)

    @property
    def cards(self):
        return self._cards

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Hand):
            return NotImplemented
        return (
            self.category == other.category
            and self.discriminator == other.discriminator
        )

    def __lt__(self, other: Hand) -> bool:
        if not isinstance(other, Hand):
            return NotImplemented
        if self.category == other.category:
            return self.category_comparator(self.discriminator, other.discriminator)
        else:
            return self.category < other.category

    def __str__(self) -> str:
        return " ".join(str(card) for card in self.cards)

    def __repr__(self) -> str:
        return f"Hand({str(self.cards)})"

    def _rank(self) -> Callable:
        return next(
            fn
            for f_name, fn in self.__class__.__dict__.items()
            if (
                f_name.startswith("is_")
                and callable(fn)
                and hasattr(fn, "category")
                and fn(self) is True
            )
        )

    @property
    def category(self):
        return self._rank().category

    @property
    def category_comparator(self):
        return self._rank().compare_function

    def _cards_by_frequency_and_rank(self):
        return tuple(
            card_and_count[0].rank for card_and_count in sorted(
                (
                    card_and_count
                    for card_and_count in self._card_counts.most_common()
                ),
                key=itemgetter(1, 0),
                reverse=True
            )
        )

    @property
    def discriminator(self):
        return self._cards_by_frequency_and_rank()

    @_categorizer(HandCategory.STRAIGHT_FLUSH, compare_function=_straight_comparator)
    def is_straight_flush(self):
        return self.is_flush() and self.is_straight()

    @_categorizer(HandCategory.FOUR_OF_A_KIND)
    def is_four_of_a_kind(self):
        return max(self._card_counts.values()) == 4

    @_categorizer(HandCategory.FULL_HOUSE)
    def is_full_house(self):
        return sorted(self._card_counts.values()) == [2, 3]

    @_categorizer(HandCategory.FLUSH)
    def is_flush(self) -> bool:
        return len(set(card.suit for card in self.cards)) == 1

    @_categorizer(HandCategory.STRAIGHT, compare_function=_straight_comparator)
    def is_straight(self):
        if self.discriminator == _SMALL_STRAIGHT:
            return True
        start = self.discriminator[0]
        return self.discriminator == tuple(range(start, start - 5, -1))

    @_categorizer(HandCategory.THREE_OF_A_KIND)
    def is_three_of_a_kind(self):
        return not self.is_full_house() and max(self._card_counts.values()) == 3

    @_categorizer(HandCategory.TWO_PAIR)
    def is_two_pair(self):
        return sorted(self._card_counts.values()) == [1, 2, 2]

    @_categorizer(HandCategory.ONE_PAIR)
    def is_one_pair(self):
        return sorted(self._card_counts.values()) == [1, 1, 1, 2]

    @_categorizer(HandCategory.HIGH_CARD)
    def is_high_card(self):
        return (
            sorted(self._card_counts.values()) == [1, 1, 1, 1, 1]
            and not self.is_straight()
        )
