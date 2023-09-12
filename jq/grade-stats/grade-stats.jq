def letter_grade:
  ([[0, (. - 50) / 10] | max, 4] | min | trunc) as $idx
  | ["F", "D", "C", "B", "A"]
  | .[$idx];


def count_letter_grades:
  reduce (.[] | letter_grade) as $letter (
           {"A": 0, "B": 0, "C": 0, "D": 0, "F": 0};
           .[$letter] += 1);
