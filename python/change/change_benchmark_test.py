import pytest

from change import _find_fewest_coins, count_dyn, count_rec


def test_large_target_values(benchmark):
    actual = benchmark(_find_fewest_coins, [1, 2, 5, 10, 20, 50, 100], 999)
    expected = [2, 2, 5, 20, 20, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100]
    assert actual == expected


def test_count_dyn(benchmark):
    n = benchmark(count_dyn, 999, [1, 2, 5, 10, 20, 50, 100])
    expected = [2, 2, 5, 20, 20, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100]
    assert n == len(expected)


def test_count_rec(benchmark):
    n = benchmark(count_rec, 999, [1, 2, 5, 10, 20, 50, 100])
    expected = [2, 2, 5, 20, 20, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100]
    assert n == len(expected)
