from collections import defaultdict


class School:
    def __init__(self):
        self._rosters = defaultdict(list)

    def add_student(self, name, grade):
        self._rosters[grade].append(name)

    def roster(self):
        return [
            student
            for grade in sorted(self._rosters.keys())
            for student in self.grade(grade)
        ]

    def grade(self, grade):
        return sorted(self._rosters[grade])
