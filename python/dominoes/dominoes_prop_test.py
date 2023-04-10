"""Check using property testing via the excellent hypothesis package."""
from collections import Counter

import pytest
from hypothesis import given, settings, strategies as st

from dominoes import can_chain

N_PIPS = 6
MAX_PIECES = 12


# Strategies

@st.composite
def chainable(draw, n_pips=N_PIPS, max_size=MAX_PIECES):
    """Create chainable pieces by constructing an explicit chain.

    Need to check if, by running permutations, we actually generate from several
    distinct chains or just permute the one.
    """
    pips = draw(st.lists(st.integers(0, n_pips), min_size=2, max_size=max_size - 1))
    pieces = list(zip(pips[:-1], pips[1:]))
    # close the chain
    pieces.append((pieces[-1][1], pieces[0][0]))
    return draw(st.permutations(pieces))


def has_odd_pips(ps):
    return any(
        map(
            lambda x: x % 2 != 0,
            Counter(pip for p in ps for pip in p).values()
        )
    )


@st.composite
def unchainable_by_odd_pips(draw, n_pips=N_PIPS, max_size=MAX_PIECES):
    """Build sets of pieces with some odd pips.

    This is unsavourly slow.
    """
    return draw(
        st.lists(
            st.tuples(st.integers(0, n_pips), st.integers(0, n_pips)),
            min_size=1,
            max_size=max_size
        ).filter(has_odd_pips)
    )


@st.composite
def unchainable(draw, n_pips=N_PIPS, max_size=MAX_PIECES):
    """Presuming we make more ways to build unchainable pieces, add them here."""
    return draw(st.one_of(unchainable_by_odd_pips(n_pips=n_pips, max_size=max_size)))


# Tests

@given(chainable())
def test_chainable(c):
    assert_correct_chain(c, can_chain(c))


@given(unchainable())
@settings(deadline=4000)  # extend deadline to 2 seconds
def test_unchainable(c):
    refute_correct_chain(c, can_chain(c))


# Utility functions adapted from dominoes_test.py

def normalize_dominoes(dominoes):
    return list(sorted(tuple(sorted(domino)) for domino in dominoes))


def assert_same_dominoes(input_dominoes, output_chain):
    msg = (
        "Dominoes used in the output must be the same "
        "as the ones given in the input"
    )
    input_normal = normalize_dominoes(input_dominoes)
    output_normal = normalize_dominoes(output_chain)
    assert input_normal == output_normal, msg


def assert_consecutive_dominoes_match(output_chain):
    for j in range(len(output_chain) - 1):
        msg = (
            f"In chain {output_chain}, right end of domino {j} ({output_chain[j]}) "
            f"and left end of domino {j + 1} ({output_chain[j + 1]}) must match"
        )
        assert output_chain[j][1] == output_chain[j + 1][0], msg


def assert_dominoes_at_ends_match(output_chain):
    msg = (
        "In chain {output_chain}, left end of first domino ({output_chain[0]}) and "
        "right end of last domino ({output_chain[-1]}) must match"
    )
    assert output_chain[0][0] == output_chain[-1][1], msg


def assert_correct_chain(input_dominoes, output_chain):
    msg = f"There should be a chain for {input_dominoes}"
    assert output_chain is not None, msg
    assert_same_dominoes(input_dominoes, output_chain)
    if not any(output_chain):
        return
    assert_consecutive_dominoes_match(output_chain)
    assert_dominoes_at_ends_match(output_chain)


def refute_correct_chain(input_dominoes, output_chain):
    msg = f"There should be no valid chain for {input_dominoes}, but got {output_chain}"
    assert output_chain is None, msg
