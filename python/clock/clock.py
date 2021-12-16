from __future__ import annotations
from dataclasses import dataclass


@dataclass
class Clock:
    hours: int
    minutes: int

    def __init__(self, hours, minutes):
        self.hours = (hours + minutes // 60) % 24
        self.minutes = minutes % 60

    def __str__(self) -> str:
        return f"{self.hours:02}:{self.minutes:02}"

    def __add__(self, minutes: int) -> Clock:
        return Clock(self.hours, self.minutes + minutes)

    def __sub__(self, minutes: int) -> Clock:
        return Clock(self.hours, self.minutes - minutes)
