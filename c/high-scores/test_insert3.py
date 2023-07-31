import pytest

from insert3 import insert_top3_v1


FUNCTIONS = {
    "v1": insert_top3_v1,
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
def test_functions(store, function_info, initial, k, expected):
    sequence = initial.copy()
    fn_id, function = function_info
    counts = function(sequence, k)
    store["counts"] = counts
    store["inputs"] = (initial, k)
    store["function"] = fn_id
    assert sequence == expected
