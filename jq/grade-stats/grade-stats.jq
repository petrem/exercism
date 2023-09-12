def letter_grade:
  ([[0, (. - 50) / 10] | max, 4] | min | trunc) as $idx
  | ["F", "D", "C", "B", "A"]
  | .[$idx];


# Given an object that maps a student's name to their grade,
# generate an object that maps the letter grade to the number of 
# students with that grade

def count_letter_grades:
  reduce (.[] | letter_grade | tostring) as $letter (
           {"A": 0, "B": 0, "C": 0, "D": 0, "F": 0};
           .[$letter] += 1);
