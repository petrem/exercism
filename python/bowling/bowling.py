"""Exercism: bowling game."""

import operator
from abc import ABCMeta, abstractmethod
from collections import deque
from itertools import chain, islice, starmap


class BowlingGame:

    """Manages a bowling 10-frame game."""

    def __init__(self):
        self._frames = chain((Frame(j) for j in range(1, 10)), (LastFrame(),))
        self._current = next(self._frames)
        self._score = 0
        self._game_ended = False
        self._bonuses = BonusQueue()

    def roll(self, pins):
        """Add a roll and compute running score."""
        if self._game_ended:
            raise RuntimeError("Game ended. Stop calling!")
        current = self._current
        current.roll(pins)
        if current.is_complete():
            self._score += sum(starmap(operator.mul, zip(current.rolls, self._bonuses)))
            self._bonuses.add_bonus_rolls(current.get_bonus_rolls())
            self._current = next(self._frames, None)
            if self._current is None:
                self._game_ended = True

    def score(self):
        """Return finalscore."""
        if self._game_ended:
            return self._score
        raise RuntimeError("Hold your horses, game didn't end yet.")


class AbstractFrame(metaclass=ABCMeta):

    """Abstract class representing a bowling frame."""

    def __init__(self, n=None):
        self.rolls = []
        self._done = False
        self._n = str(n) or "?"  # Used only for __repr__

    def roll(self, pins):
        """Add a roll to the frame, check and update state."""
        if self._done:
            raise RuntimeError("Should have moved to next frame! This is a bug!")
        if 0 <= pins <= 10:
            self.rolls.append(pins)
            self._check_and_update_state()
        else:
            raise ValueError(f"Invalid number of pins: {pins} ∉ [0, 10]")

    def is_strike(self):
        """Return True if a strike."""
        return sum(self.rolls[:1]) == 10

    def is_spare(self):
        """Return True if a spare."""
        return sum(self.rolls[:2]) == 10 and not self.is_strike()

    def is_mark(self):
        """Return True if a mark."""
        return self.is_strike() or self.is_spare()

    def is_open(self):
        """Return True if an open frame."""
        return not self.is_mark()

    def is_complete(self):
        """Return True if frame is complete."""
        return self._done

    @abstractmethod
    def __repr__(self):
        raise NotImplementedError()

    @abstractmethod
    def _check_and_update_state(self):
        raise NotImplementedError()

    @abstractmethod
    def get_bonus_rolls(self):
        """Return bonuses depending on rolls and frame type."""
        raise NotImplementedError()


class Frame(AbstractFrame):

    """Non-last frame in a bowling round."""

    def __repr__(self):
        return f"Frame{self._n}{self.rolls}"

    def _check_and_update_state(self):
        if sum(self.rolls) > 10:
            raise ValueError(f"Invalid pins counts in frame: {self.rolls}")
        if len(self.rolls) == 2 or self.is_strike():
            self._done = True

    def get_bonus_rolls(self):
        """Return bonus rolls according to mark type."""
        assert self.is_complete()
        if self.is_strike():
            return 2
        if self.is_spare():
            return 1
        return 0


class LastFrame(AbstractFrame):

    """Last frame in a bowling round.

    Extends with bonus rolls for spare/strike scored at face value.
    """

    def __repr__(self):
        return f"LastFrame{self.rolls}"

    def _check_and_update_state(self):
        n_rolls = len(self.rolls)
        if any(
            (
                self.is_spare() and sum(self.rolls[:2]) > 10,
                self.is_strike()
                and 10 not in self.rolls[1:]
                and sum(self.rolls[1:]) > 10,
                self.is_strike()
                and n_rolls == 3
                and self.rolls[1] != 10
                and self.rolls[2] == 10,
                self.is_open() and sum(self.rolls) > 10,
            )
        ):
            raise ValueError(f"Invalid pins counts in frame: {self.rolls}")
        if n_rolls == 3 if self.is_mark() else n_rolls == 2:
            self._done = True

    def get_bonus_rolls(self):
        """Return 0 (no extra-counts in last frame)."""
        return 0


class BonusQueue:

    """An queue returning accumulated bonuses, or 1 forever."""

    def __init__(self):
        self._queue = deque()

    def __iter__(self):
        return self

    def __next__(self):
        if self._queue:
            return self._queue.popleft()
        return 1

    def __repr__(self):
        left = list(self._queue)
        return f"BonusQueue({left})"

    def add_bonus_rolls(self, positions):
        """Update bonuses for next `positions` rolls."""
        self._queue = deque(islice((bonus + 1 for bonus in self), positions))
