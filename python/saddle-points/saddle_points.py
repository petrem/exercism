"""Exercise: saddle-points"""
from collections import defaultdict


def saddle_points(matrix):
    """Find saddle points in matrix."""
    if matrix == []:
        return []
    n_cols = len(matrix[0])
    if any(len(row) != n_cols for row in matrix[1:]):
        raise ValueError("irregular matrix")
    rows_candidates = defaultdict(set)
    cols_candidates = defaultdict(set)
    cols_min = list(matrix[0])
    for i, row in enumerate(matrix, start=1):
        row_max = row[0]
        for j, elem in enumerate(row, start=1):
            if elem > row_max:
                row_max = elem
                rows_candidates[i] = {(i, j)}
            elif elem == row_max:
                rows_candidates[i].add((i, j))
            if elem < cols_min[j - 1]:
                cols_min[j - 1] = elem
                cols_candidates[j] = {(i, j)}
            elif elem == cols_min[j - 1]:
                cols_candidates[j].add((i, j))
    result = set.intersection(
        set.union(*rows_candidates.values()),
        set.union(*cols_candidates.values()),
    )
    return [{"row": r, "column": c} for (r, c) in result]
