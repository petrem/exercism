"""Scale generator.

As usual, an over-engineered solution -- having fun modelling with Enums and whatnot.

*Warnings*:

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
    PitchClass,
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
        return [
            self._strategy.get_note(pc).__ascii__()
            for pc in self._make_scale(repeat(m2, 12))
        ][:-1]

    def interval(self, intervals: str) -> list[str]:
        """Generate scale based on provided intervals."""
        return [
            self._strategy.get_note(pc).__ascii__()
            for pc in self._make_scale(map(self._to_interval, intervals))
        ][:-1]

    def _make_scale(self, intervals: Iterable[Interval]) -> Iterable[PitchClass]:
        yield from accumulate(intervals, initial=self.tonic.pitch_class)

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
