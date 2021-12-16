def slices(series, length):
    if not isinstance(series, str) or not series:
        raise ValueError("Argument `series` is not iterable or is empty")
    if not isinstance(length, int) or length <= 0 or length > len(series):
        raise ValueError(
            "Argument `length` is not a positive int smaller that len(series)"
        )
    return [series[i:i+length] for i in range(len(series) - length + 1)]
