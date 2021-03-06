"""Exercism: Diffie-Hellman."""
import random

random.seed(1337)


def private_key(p):
    """Generate a random private key in (1, `p`). `p` must be a prime number."""
    return random.randint(2, p - 1)


def public_key(p, g, private):
    """Generate public key from `private`.

    `private` should have been generated smaller than `p`. `g` is another prime.

    """
    return g ** private % p


def secret(p, public, private):
    """Generate a shared secret.

    `private` is your own key and `public` should have been generated by your buddy
    using the same primes you agreed upon.
    """
    return public ** private % p
