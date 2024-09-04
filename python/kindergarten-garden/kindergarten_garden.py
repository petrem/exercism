"""Kindergarten Garden exercise."""

from typing import ClassVar


DEFAULT_STUDENTS = (
    "Alice",
    "Bob",
    "Charlie",
    "David",
    "Eve",
    "Fred",
    "Ginny",
    "Harriet",
    "Ileana",
    "Joseph",
    "Kincaid",
    "Larry",
)


class Garden:  # pylint: disable=too-few-public-methods
    """Representation for student's garden."""

    PLANTS: ClassVar = {
        "C": "Clover",
        "G": "Grass",
        "R": "Radishes",
        "V": "Violets",
    }

    PLANTS_PER_STUDENT_PER_ROW = 2
    PLANT_ROWS = 2, "two"

    def __init__(self, diagram, students=DEFAULT_STUDENTS):
        self._students = sorted(students)
        self._diagram = diagram.splitlines()
        n_rows, n_rows_str = self.PLANT_ROWS
        if (
            len(self._diagram) != n_rows
            or len({len(row) for row in self._diagram}) != 1
            or len(self._diagram[0]) % 2 != 0
        ):
            raise ValueError(
                f"Diagram should have {n_rows_str} even, equal length lines"
            )

    def plants(self, student):
        """List of student's plants."""
        pos = self._students.index(student) * self.PLANTS_PER_STUDENT_PER_ROW
        student_slice = slice(pos, pos + self.PLANTS_PER_STUDENT_PER_ROW)
        return [
            self.PLANTS[plant] for row in self._diagram for plant in row[student_slice]
        ]
