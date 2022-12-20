"""Scale generator.

As usual, an over-engineered solution -- having fun modelling with Enums and whatnot.

*Warnings*:

  * This solution uses python 3.11 features! This is why it fails the tests.
  * This solution is, to some degree, based on pitch-class set theory, of which I
    know practically nothing. Don't learn from me.
  * It only makes sense in equal temparament (as the exercise itself).
  * Large parts of the model code are not really used in the solution. Just explored.

"""

from itertools import accumulate, repeat
from typing import Iterable

from model import (
    A2,
    M2,
    AbstractPC2NoteStrategy,
    Accidental,
    Interval,
    Note,
    NoteName,
    UseNaturalOrFlatsStrategy,
    UseNaturalOrSharpsStrategy,
    m2,
)


class Scale:

    """Solution to 'scale generator' exercise."""

    WITH_SHARPS = frozenset(
        ["C", "G", "D", "A", "E", "B", "F#", "a", "e", "b", "f#", "c#", "g#", "d#"]
    )
    WITH_FLATS = frozenset(
        ["F", "Bb", "Eb", "Ab", "Db", "Gb", "d", "g", "c", "f", "bb", "eb"]
    )
    SCALE_STRATEGY: dict[str, AbstractPC2NoteStrategy] = {
        **{t: UseNaturalOrSharpsStrategy() for t in WITH_SHARPS},
        **{t: UseNaturalOrFlatsStrategy() for t in WITH_FLATS},
    }

    def __init__(self, tonic: str):
        assert len(tonic) > 0, f"{tonic} should not be empty"
        self.tonic = Note(NoteName(tonic[0]), Accidental(tonic[1:]))
        self._strategy = self.SCALE_STRATEGY[tonic]

    def chromatic(self) -> list[str]:
        """Generate chromatic scale."""
        return list(self._make_scale(repeat(m2, 11)))

    def interval(self, intervals: str) -> list[str]:
        """Generate scale based on provided intervals."""
        return list(self._make_scale(map(self._to_interval, intervals)))

    def _make_scale(self, intervals: Iterable[Interval]) -> Iterable[str]:
        yield from (
            self._strategy.get_note(pc).__ascii__()
            for pc in accumulate(intervals, initial=self.tonic.pitch_class)
        )

    @staticmethod
    def _to_interval(interval: str) -> Interval:
        try:
            return {
                "m": m2,
                "M": M2,
                "A": A2,
            }[interval]
        except KeyError as e:
            raise ValueError(f"Cannot create interval from {interval}") from e
