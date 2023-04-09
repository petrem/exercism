"""Check if dominoes can be chained.

This solution is using recursive generators, as it felt the most natural way to model it
in a first attempt. Recursion can go too deep for python, so it should be rewritten
without recursion.

There is a lot of lists copying going on, that also should be avoided, and the
partitioning by matching / not matching dominoes is questionable.

And of course, there are most likely better algorithms to do it (at the very least,
could dynamic programming be applied?).

"""
from itertools import tee
from typing import List, Tuple, Union, Iterable, Callable

Domino = Tuple[int, int]
DominoChain = List[Domino]


def can_chain(dominoes: DominoChain) -> Union[DominoChain, None]:
    return next(
        (
            domino_chain
            for domino_chain in build_chain([], dominoes)
            if not domino_chain or are_matching(domino_chain[-1], domino_chain[0])
        ),
        None
    )


def are_matching(d1: Domino, d2: Domino) -> bool:
    """Return whether dominoes match.

    Inefficient (function call overhead too large for operation).
    """
    return d1[1] == d2[0]


def partition(p: Callable, xs: Iterable) -> Tuple[Iterable, Iterable]:
    cond1, cond2 = tee((p(x), x) for x in xs)
    return [x for cond, x in cond1 if cond], [x for cond, x in cond2 if not cond]


def flip(d: Domino) -> Domino:
    """Return the domino, rotated.

    Inefficient (function call overhead too large for operation).
    """
    return d[1], d[0]


def build_chain(partial, available):
    """Generate all chains of matching dominoes from ``available``.

    If ``available`` is empty, will yield an empty chain.
    If dominoes cannot be chain, will yield nothing.
    """
    print(f"DEBUG: {partial=}, {available=}")
    if not available:
        yield partial

    if not partial:
        for idx in range(len(available)):
            others = available.copy()
            d = others.pop(idx)
            yield from build_chain([d], others)
    else:
        previous = partial[-1]
        matching, rest = partition(
            lambda x: are_matching(previous, x) or are_matching(previous, flip(x)),
            available
        )

        # todo: since we do this, maybe useless to partition by matching?
        for idx in range(len(matching)):
            remaining_matches = matching.copy()
            d = remaining_matches.pop(idx)
            others = remaining_matches + rest
            print(f"DEBUG: attempt {d}, {others=}")
            if are_matching(previous, d):
                print(f"DEBUG: matching {d}")
                yield from build_chain(partial + [d], others)
            if are_matching(previous, (d_flipped := (d[1], d[0]))):
                print(f"DEBUG: matching {d_flipped}")
                yield from build_chain(partial + [d_flipped], others)
