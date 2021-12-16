import unittest

import pytest

from say import say, groups_of_digits, groups_of_digits_reversed


# Tests adapted from `problem-specifications//canonical-data.json` @ v1.2.0

class SayTest(unittest.TestCase):
    def test_zero(self):
        self.assertEqual(say(0), "zero")

    def test_one(self):
        self.assertEqual(say(1), "one")

    def test_fourteen(self):
        self.assertEqual(say(14), "fourteen")

    def test_twenty(self):
        self.assertEqual(say(20), "twenty")

    def test_twenty_two(self):
        self.assertEqual(say(22), "twenty-two")

    def test_one_hundred(self):
        self.assertEqual(say(100), "one hundred")

    # additional track specific test
    def test_one_hundred_twenty_three(self):
        self.assertEqual(say(123), "one hundred and twenty-three")

    def test_one_thousand(self):
        self.assertEqual(say(1000), "one thousand")

    def test_one_thousand_two_hundred_thirty_four(self):
        self.assertEqual(say(1234), "one thousand two hundred and thirty-four")

    def test_one_million(self):
        self.assertEqual(say(1000000), "one million")

    def test_1002345(self):
        self.assertEqual(
            say(1002345),
            "one million two thousand three hundred and forty-five")

    def test_one_billion(self):
        self.assertEqual(say(1000000000), "one billion")

    def test_987654321123(self):
        self.assertEqual(
            say(987654321123), ("nine hundred and eighty-seven billion "
                                "six hundred and fifty-four million "
                                "three hundred and twenty-one thousand "
                                "one hundred and twenty-three"))

    def test_number_too_large(self):
        with self.assertRaisesWithMessage(ValueError):
            say(1000000000000)

    def test_number_negative(self):
        with self.assertRaisesWithMessage(ValueError):
            say(-1)

    # Utility functions
    def setUp(self):
        try:
            self.assertRaisesRegex
        except AttributeError:
            self.assertRaisesRegex = self.assertRaisesRegexp

    def assertRaisesWithMessage(self, exception):
        return self.assertRaisesRegex(exception, r".+")


@pytest.mark.parametrize(
    "test_input,expected", [
        (9, ["9"]), (89, ["89"]), (789, ["789"]),
        (6789, ["6", "789"]), (56789, ["56", "789"]), (456789, ["456", "789"]),
        (3456789, ["3", "456", "789"]), (23456789, ["23", "456", "789"]), (123456789, ["123", "456", "789"]),
    ]
)
def test_groups_of_digits(test_input, expected):
    assert list(groups_of_digits(test_input)) == expected


@pytest.mark.parametrize(
    "test_input,expected", [
        (9, ["9"]), (89, ["89"]), (789, ["789"]),
        (6789, ["789", "6"]), (56789, ["789", "56"]), (456789, ["789", "456"]),
        (3456789, ["789", "456", "3"]), (23456789, ["789", "456", "23"]), (123456789, ["789", "456", "123"]),
    ]
)
def test_groups_of_digits_reversed(test_input, expected):
    assert list(groups_of_digits_reversed(test_input)) == expected
