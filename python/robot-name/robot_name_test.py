import unittest
import random

from robot_name import Robot, _name_from_random_value, remainders


class GenerateNameTest(unittest.TestCase):
    def test__generate_names(self):
        name_format = "LD"
        size = 26 * 10
        all_names = [_name_from_random_value(name_format, i) for i in range(size)]
        self.assertEqual(len(all_names), len(set(all_names)))
        self.assertEqual(all_names[0], "A0")
        self.assertEqual(all_names[size-1], "Z9")

    def test_reminders(self):
        result = list(remainders(5, [1]))
        self.assertEqual(result, [0])
        result = list(remainders(123, [3, 3, 3, 3, 3]))
        self.assertEqual(result, [0, 2, 1, 1, 1])


class RobotNameTest(unittest.TestCase):
    # assertRegex() alias to adress DeprecationWarning
    # assertRegexpMatches got renamed in version 3.2
    if not hasattr(unittest.TestCase, "assertRegex"):
        assertRegex = unittest.TestCase.assertRegexpMatches

    name_re = r'^[A-Z]{2}\d{3}$'

    def test_has_name(self):
        self.assertRegex(Robot().name, self.name_re)

    def test_name_sticks(self):
        robot = Robot()
        robot.name
        self.assertEqual(robot.name, robot.name)

    def test_different_robots_have_different_names(self):
        self.assertNotEqual(
            Robot().name,
            Robot().name
        )

    def test_reset_name(self):
        # Set a seed
        seed = "Totally random."

        # Initialize RNG using the seed
        random.seed(seed)

        # Call the generator
        robot = Robot()
        name = robot.name

        # Reinitialize RNG using seed
        random.seed(seed)

        # Call the generator again
        robot.reset()
        name2 = robot.name
        self.assertNotEqual(name, name2)
        self.assertRegex(name2, self.name_re)


if __name__ == '__main__':
    unittest.main()
