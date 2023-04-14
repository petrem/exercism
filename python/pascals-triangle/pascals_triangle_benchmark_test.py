import pytest

from pascals_triangle import rows


def test_300_rows(benchmark):
    benchmark(rows, 300)
