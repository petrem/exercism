from collections import defaultdict
from itertools import groupby
from operator import itemgetter

import pytest


count_data_key = pytest.StashKey[dict]()
RESULTS = defaultdict(list)


@pytest.hookimpl(tryfirst=False, hookwrapper=True)
def pytest_runtest_makereport(item, call):
    global RESULTS
    outcome = yield
    test_report = outcome.get_result()

    if test_report.when == "call":
        stored_info = item.stash[count_data_key]
        RESULTS[stored_info["function"]].append(stored_info)


def get_operations(info):
    return info["counts"].operations


def pytest_sessionfinish(session, exitstatus):
    print("=== Operations counts ===")
    for fn, data in RESULTS.items():
        print(fn)
        sorted_data = sorted(data, key=get_operations)
        by_ops_count = groupby(sorted_data, key=get_operations)
        for ops_count, info_list in by_ops_count:
            by_seq = groupby(
                sorted(info['inputs'] for info in info_list), key=itemgetter(0)
            )
            groupped = "\n\t\t".join(
                f"{seq}:\t{', '.join(str(seq_k[1]) for seq_k in ins)}" for seq, ins in by_seq
            )
            print(f"\t{ops_count}:\t{groupped}")


@pytest.fixture(scope="function")
def store(request):
    data = {}
    request.node.stash[count_data_key] = data
    return data
