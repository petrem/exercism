"""Exercise: alphametics."""
from itertools import permutations
from string import ascii_uppercase, digits


def solve(puzzle: str) -> dict[str, int] | None:
    """Solve alphametics puzzle.

    Assumes the puzzle is correct.
    """
    left, result = puzzle.replace(" ", "").split("==")
    terms = left.split("+")
    first_letters = {t[0] for t in terms}
    first_letters.add(result[0])
    letters = "".join(
        sorted(
            set(puzzle).intersection(ascii_uppercase), key=first_letters.__contains__
        )
    )
    n_letters = len(letters)
    n_firsts = len(first_letters)

    solutions = (
        dict(zip(letters, map(int, candidate), strict=True))
        for candidate in permutations(digits, n_letters)
        if "0" not in candidate[-n_firsts:]
        if sum(
            int(term.translate(trans := str.maketrans(letters, "".join(candidate))))
            for term in terms
        )
        == int(result.translate(trans))
    )
    return next(solutions, None)
