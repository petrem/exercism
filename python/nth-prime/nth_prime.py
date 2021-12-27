"""Calculate the nth prime number."""
import math
from itertools import compress, islice, repeat


def prime(number):
    """Return the `number` prime number."""
    if number == 0:
        raise ValueError("there is no zeroth prime")
    if number < 0:
        raise ValueError("negative nth??")
    if number < 6:
        return [2, 3, 5, 7, 11][number - 1]
    upper_bound = int(0.5 + number * (math.log(number) + math.log(math.log(number))))
    return nth(sieve(upper_bound), number - 1)


def sieve(limit):
    """Return an iterable of the primes upto `limit`."""
    candidates = list(range(2, limit + 1))
    n_candidates = len(candidates)
    isprime = [True] * n_candidates
    for factor in candidates[: int(math.sqrt(limit))]:
        if isprime[factor - 2]:
            from_ = 2 * factor - 2
            how_many = (n_candidates - from_ - 1) // factor + 1
            isprime[from_::factor] = repeat(False, how_many)
    return compress(candidates, isprime)


def nth(iterable, n, default=None):
    """Return the nth item or a default value."""
    return next(islice(iterable, n, None), default)
