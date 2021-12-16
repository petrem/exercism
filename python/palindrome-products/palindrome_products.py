from itertools import combinations_with_replacement
from functools import partial


def _is_palindrome(n: int) -> bool:
    n_str = str(n)
    return n_str == n_str[::-1]


def _all_palindromes(max_factor: int, min_factor: int) -> dict:
    all_palindromes = {}
    for palindrome, factors in (
        (a * b, (a, b))
        for a, b in combinations_with_replacement(
            range(min_factor, max_factor + 1),
            2
        )
        if _is_palindrome(a * b)
    ):
        all_palindromes.setdefault(palindrome, []).append(factors)
    return all_palindromes


def peak_palindrome(fn, max_factor, min_factor=0):
    if max_factor < min_factor:
        raise ValueError("Naugty, min_factor > max_factor")
    palindromes = _all_palindromes(max_factor, min_factor)
    if not palindromes:
        return None, []
    peak = fn(palindromes)
    return peak, palindromes[peak]


smallest_palindrome = partial(peak_palindrome, min)
largest_palindrome = partial(peak_palindrome, max)
