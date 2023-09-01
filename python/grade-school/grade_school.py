"""Exercism: python: grade-school

What I'd expect to be a typical solution.
"""
from bisect import insort


class School:
    """School grades roster."""

    def __init__(self):
        self._rosters = {}
        self._added = []

    def add_student(self, name, grade):
        """Add student to the roster, if not already present.

        Records a trail of successfull additions.

        :param name: Student name.
        :type name: str

        :param grade: Grade for the student.
        :type grade: int

        :return: Nothing.
        :rtype: None
        """

        if name not in self.roster():
            roster = self._rosters.setdefault(grade, [])
            insort(roster, name)
            self._added.append(True)
        else:
            self._added.append(False)

    def roster(self):
        """Get the full list of students from the roster.

        :return: List of students, ordered by grade and name.
        :rtype: list[str]
        """
        return [
            name for grade in sorted(self._rosters.keys()) for name in self.grade(grade)
        ]

    def grade(self, grade):
        """Get students having the specified grade.

        :param grade: The grade for which a roster is requested.
        :type grade: int

        :return: The list of students for the specified grade.
        :rtype: list[str]
        """

        if grade in self._rosters:
            return list(self._rosters[grade])
        return []

    def added(self):
        """Get the trail for :method:``add_student``.

        :return: List of bools indicating successful additions.
        :rtype: list[bool]
        """

        return list(self._added)
