from abc import ABCMeta, abstractmethod
from itertools import chain, islice
from functools import partial
import sys


def recite(start: int, take: int = 1) -> str:
    stanzas = chain(
        (partial(GenericStanza, i) for i in range(99, 1, -1)),
        (partial(OneBottleStanza, 1), partial(NoBottlesStanza, 0))
    )
    return list(
        chain(
            *intersperse(
                ("", ),
                (
                    stanza().verses
                    for stanza in islice(stanzas, 99 - start, 99 - start + take)
                )
            )
        )
    )


def intersperse(sep, iterable):
    iterator = iter(iterable)
    yield next(iterator)
    for e in iterator:
        yield sep
        yield e


class BeerOnWallMixin:

    @property
    def beers(self):
        return f"{self} of beer"

    @property
    def beers_on_wall(self):
        return f"{self.beers} on the wall"


class ManyBottles(BeerOnWallMixin):

    def __init__(self, how_many):
        self.how_many = how_many

    def __str__(self):
        return f"{self.how_many} bottles"


class OneBottle(BeerOnWallMixin):

    def __str__(self):
        return "1 bottle"


class NoMoreBottles(BeerOnWallMixin):

    def __str__(self):
        return "no more bottles"


def make_bottles(how_many):
    return {
        0: NoMoreBottles(),
        1: OneBottle(),
    }.get(how_many, ManyBottles(how_many))


class Stanza(metaclass=ABCMeta):

    def __init__(self, how_many_bottles):
        self.current_bottles = make_bottles(how_many_bottles)
        self.remaining_bottles = make_bottles((how_many_bottles - 1) % 100)
        print(f"Called with {how_many_bottles}")

    @property
    def verses(self):
        return (
            self.versify(
                self.current_bottles.beers_on_wall.capitalize(),
                self.current_bottles.beers
            ),
            self.versify(
                self.action(),
                self.remaining_bottles.beers_on_wall
            )
        )

    @staticmethod
    def versify(part1, part2):
        return f"{part1}, {part2}."

    @abstractmethod
    def action(self):
        raise NotImplementedError


class GenericStanza(Stanza):

    def action(self):
        return "Take one down and pass it around"


class OneBottleStanza(Stanza):

    def action(self):
        return "Take it down and pass it around"


class NoBottlesStanza(Stanza):

    def action(self):
        return "Go to the store and buy some more"


if __name__ == "__main__":
    print(recite(int(sys.argv[1]), int(sys.argv[2])))
