# simple dynamic programming solution
def maximum_value(W, items):
    row = [0] * (W+1)
    for item in items:
        row = [*[row[w] for w in range(0, min(item["weight"], W+1))], *[max(row[w], row[w-item["weight"]]+item["value"]) for w in range( min(item["weight"], W+1), W+1)]]
    return row[-1]
