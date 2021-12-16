def divisors():
    """Yield the next odd divisor to test. Begin with a few well-known primes"""
    for i in [2, 3, 5, 7, 11, 13]:
        yield i
    next_ = 17
    while True:
        yield next_
        next_ += 2


def prime_factors(natural_number):
    if natural_number == 1:
        return []
    factors = []
    n = natural_number
    for q in divisors():
        while n % q == 0:
            r = n // q
            factors.append(q)
            if q * q > r:
                if r != 1:
                    factors.append(r)
                return factors
            n = r
