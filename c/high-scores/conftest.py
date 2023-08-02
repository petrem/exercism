from collections import defaultdict
from itertools import groupby
from operator import itemgetter

import pytest


COUNT_DATA_KEY = pytest.StashKey[dict]()
# RESULTS = defaultdict(list)
RESULTS = []


@pytest.hookimpl(tryfirst=False, hookwrapper=True)
def pytest_runtest_makereport(item, call):
    """Collect metadata that was stored in the stash by tests into global ``RESULTS``.

    There's probably a better way to store this info directly, somewhere.
    """
    outcome = yield
    test_report = outcome.get_result()

    if test_report.when == "call":
        stored_info = item.stash[COUNT_DATA_KEY]
        # RESULTS[stored_info["function"]].append(stored_info)
        RESULTS.append(stored_info)


def _get_operations(info):
    """Get numberof operations out of metadata."""
    return info["counts"].operations


def _groupby(iterable, key):
    """Just like ``groupby`` but sort iterable, as convenience."""
    yield from groupby(sorted(iterable, key=key), key=key)


def report_operations_test_top3_function(metadata):
    for ops_count, info_list in _groupby(metadata, key=_get_operations):
        by_seq = _groupby((info['inputs'] for info in info_list), key=itemgetter(0))
        groupped = "\n\t\t".join(
            f"{seq}:\t{', '.join(str(seq_and_k[1]) for seq_and_k in ins)}"
            for seq, ins in by_seq
        )
        print(f"\t{ops_count} ops:\t{groupped}")


def report_operations_test_get_top3(metadata):
    for info in sorted(metadata, key=_get_operations):
        inputs = "; ".join(
            ",".join(str(x) for x in [*xs[:4], "...", *xs[-4:]])
            for xs in info["inputs"]
        )
        sorted_sign = (
            "(*)"
            if (xs := list(info["inputs"][0])) == sorted(xs)
            or xs == sorted(xs, reverse=True)
            else ""
        )
        print(f"\t{_get_operations(info):4d} ops:\t{inputs}{sorted_sign}")


_REPORT_BY_TEST_NAME = {
    "test_top3_function": report_operations_test_top3_function,
    "test_get_top3": report_operations_test_get_top3,
}


def pytest_sessionfinish(session, exitstatus):
    print("\n=== Operations counts ===")
    # display operations counts by test function name and by the insert_top3_vNN
    # that was used
    for test, test_metadata in _groupby(RESULTS, itemgetter("test")):
        print(f"--- {test} ---")
        report = _REPORT_BY_TEST_NAME.get(test, lambda *args: print(f"No report for {test}"))
        for function, metadata in _groupby(test_metadata, itemgetter("function")):
            metadata = list(metadata)
            total_ops = sum(_get_operations(info) for info in metadata)
            print(f"{function}: Total {total_ops}")
            report(metadata)


@pytest.fixture(scope="function")
def store_meta(request):
    data = {"test": request.node.originalname}
    request.node.stash[COUNT_DATA_KEY] = data
    def set(*, counts, inputs, function):
        nonlocal data
        data.update(counts=counts, inputs=inputs, function=function)
    return set
