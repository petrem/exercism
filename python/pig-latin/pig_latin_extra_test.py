import pytest

from pig_latin import translate


def test_one_letter_word():
    assert translate("b") == "bay"


def test_pig_devowelay():
    assert translate("sknch") == "sknchay"
