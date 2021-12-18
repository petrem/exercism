"""Exercise: Making the grade."""
from collections import deque
from itertools import count

FAIL_THRESHOLD = 40


def round_scores(student_scores):
    """Round scores.

    :param student_scores: list of student exam scores as float or int.
    :return: list of student scores *rounded* to nearest integer value.
    """

    return [round(x) for x in student_scores]


def _ilen(iterable):
    """Iterable length.

    Stolen from https://more-itertools.readthedocs.io/en/stable/_modules/\
    more_itertools/more.html#ilen
    """
    counter = count()
    deque(zip(iterable, counter), maxlen=0)
    return next(counter)


def count_failed_students(student_scores):
    """Count failed students.

    :param student_scores: list of integer student scores.
    :return: integer count of student scores at or below 40.
    """

    return _ilen(filter(lambda s: s <= FAIL_THRESHOLD, student_scores))


def above_threshold(student_scores, threshold):
    """Return scores not below threshold.

    :param student_scores: list of integer scores
    :param threshold :  integer
    :return: list of integer scores that are at or above the "best" threshold.
    """

    return [s for s in student_scores if s >= threshold]


def letter_grades(highest):
    """Bizzare american thing.

    (Nice and easy to get an A if your classmates are in [not so bright, lazy]).

    :param highest: integer of highest exam score.
    :return: list of integer lower threshold scores for each D-A letter grade interval.
             For example, where the highest score is 100, and failing is <= 40,
             The result would be [41, 56, 71, 86]:

             41 <= "D" <= 55
             56 <= "C" <= 70
             71 <= "B" <= 85
             86 <= "A" <= 100
    """

    grades_range = highest - FAIL_THRESHOLD
    if grades_range < 4:
        raise ValueError("Class of idiots 2021, or just a bad teacher?")
    grades_delta = grades_range // 4
    return list(range(41, highest, grades_delta))


def student_ranking(student_scores, student_names):
    """Match students and rankings and add rank.

    :param student_scores: list of scores in descending order.
    :param student_names: list of names in descending order by exam score.
    :return: list of strings in format ["<rank>. <student name>: <score>"].
    """

    return [
        f"{rank}. {name}: {score}"
        for rank, (name, score) in enumerate(
            zip(student_names, student_scores), start=1
        )
    ]


def perfect_score(student_info):
    """Find the nerd.

    :param student_info: list of [<student name>, <score>] lists
    :return: first `[<student name>, 100]` or `[]` if no student score of 100 is found.
    """
    return next((si for si in student_info if si[1] == 100), [])
