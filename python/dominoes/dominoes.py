"""Check if dominoes can be chained.

This solution is using recursive generators, as it felt the most natural way to model it
in a first attempt. Recursion can go too deep for python, so it should be rewritten
without recursion.

There is a lot of lists copying going on, that also should be avoided.

And of course, there are most likely better algorithms to do it (at the very least,
could dynamic programming be applied?).

"""
from functools import partial
from typing import List, Tuple, Union, Generic, TypeVar

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


def build_chain(partial_chain, available):
    """Generate all chains of matching dominoes from ``available``.

    If ``available`` is empty, will yield an empty chain.
    If dominoes cannot be chain, will yield nothing.
    """
    if not available:
        yield partial_chain

    # when not having a previous piece, pick any available
    match_fn = (
        partial(are_matching, partial_chain[-1]) if partial_chain else lambda x: True
    )
    for index, d in enumerate(available):
        d_flipped = (d[1], d[0])
        if match_fn(d) or match_fn(d_flipped):
            others = available.copy()
            del others[index]
            if match_fn(d):
                yield from build_chain(partial_chain + [d], others)
            if match_fn(d_flipped):
                yield from build_chain(partial_chain + [d_flipped], others)


Node = TypeVar("Node")


class Graph(Generic[T]):
    def __init__(
        self,
        nodes: Optional[List[Node]] = None,
        arcs: Optional[List[Tuple[Node, Node]]] = None,
    ):
        self.nodes = list(nodes) if nodes else []
        self.arcs = list(arcs) if arcs else []
        self.outgoing_arcs = {vi: vj for vi, vj in arcs}
        self.outgoing_arcs = {vj: vi for vi, vj in arcs}

    def get_outgoing_from(self, node):
        return
        
