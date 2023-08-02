import random
from itertools import permutations, repeat

import pytest

from insert3 import get_top3, insert_top3_v1, insert_top3_v2, insert_top3_v3


FUNCTIONS = {
    "v1": insert_top3_v1,
    "v2": insert_top3_v2,
    "v3": insert_top3_v3,
}


@pytest.fixture(params=FUNCTIONS.items(), ids=FUNCTIONS)
def function_info(request):
    return request.param


@pytest.mark.parametrize(
    "initial,k,expected",
    [
        # [3, 5, 7]
        ([3, 5, 7], 2, [3, 5, 7]),
        ([3, 5, 7], 3, [3, 5, 7]),
        ([3, 5, 7], 4, [4, 5, 7]),
        ([3, 5, 7], 5, [5, 5, 7]),
        ([3, 5, 7], 6, [5, 6, 7]),
        ([3, 5, 7], 7, [5, 7, 7]),
        ([3, 5, 7], 8, [5, 7, 8]),
        # [3, 3, 7]
        ([3, 3, 7], 2, [3, 3, 7]),
        ([3, 3, 7], 3, [3, 3, 7]),
        ([3, 3, 7], 4, [3, 4, 7]),
        ([3, 3, 7], 7, [3, 7, 7]),
        ([3, 3, 7], 8, [3, 7, 8]),
        # [5, 5, 5]
        ([5, 5, 5], 2, [5, 5, 5]),
        ([5, 5, 5], 5, [5, 5, 5]),
        ([5, 5, 5], 8, [5, 5, 8]),
        # [1, 1, 1]
        ([1, 1, 1], 2, [1, 1, 2]),
        # [1, 2, 2]
        ([1, 2, 2], 2, [2, 2, 2]),
        ([1, 2, 2], 3, [2, 2, 3]),
    ]
)
def test_top3_function(store_meta, function_info, initial, k, expected):
    sequence = initial.copy()
    fn_id, function = function_info
    counts = function(sequence, k)
    store_meta(counts=counts, inputs=(initial, k), function=fn_id)
    assert sequence == expected


def scores_and_expected():
    """Generate all permutations of a random list of scores.

    Yield these along with the expected top 3 scores.
    """
    random.seed(1337)
    scores = random.choices(range(1, 100), k=25)
    top3 = sorted(scores)[-3:]
    yield sorted(scores), top3
    yield sorted(scores, reverse=True), top3
    # and use some shuffles too
    for _ in range(0, 10):
        random.shuffle(scores)
        yield scores.copy(), top3


@pytest.mark.parametrize("scores,expected", scores_and_expected())
def test_get_top3(store_meta, function_info, scores, expected):
    fn_id, function = function_info
    scores_list = scores.copy()  # todo: is this really needed?
    top3, counts = get_top3(scores_list, function)
    store_meta(counts=counts, inputs=(scores,), function=fn_id)
    assert top3 == expected
