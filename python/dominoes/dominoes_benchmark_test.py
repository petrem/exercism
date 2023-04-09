import pytest

from dominoes import can_chain


@pytest.mark.parametrize(
    "example",
    [
        # example0: set that cannot be chained
        [
            (6, 1),
            (1, 3),
            (3, 6),
            (6, 6),
            (6, 3),
            (3, 4),
            (4, 4),
            (4, 5),
            (5, 2),
            (2, 4),
            (4, 4),
            (4, 3),
        ],
        # example1: same set with aditional piece, making it chain-able
        [
            (6, 1),
            (1, 3),
            (3, 6),
            (6, 6),
            (6, 3),
            (3, 4),
            (4, 4),
            (4, 5),
            (5, 2),
            (2, 4),
            (4, 4),
            (4, 3),
            (3, 6),
        ],
    ],
)
def test_chain_of_12(benchmark, example):
    benchmark(can_chain, example)
