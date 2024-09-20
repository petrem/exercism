//! Kindergarten Garden::Version 2::Goodbye vectors, hello iterators.

/// Determine the plants belonging to `student` given a `diagram`.
pub fn plants(diagram: &str, student: &str) -> Vec<&'static str> {
    diagram
        .lines()
        .flat_map(student_plants_getter(student))
        .collect()
}

/// List of students, in alphabetic order.
pub const STUDENTS: [&str; 12] = [
    "Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph",
    "Kincaid", "Larry",
];

fn get_plant_name(repr: char) -> &'static str {
    match repr {
        'C' => "clover",
        'G' => "grass",
        'R' => "radishes",
        'V' => "violets",
        _ => panic!("Uh oh, unknown plant, could be poisonous!"),
    }
}

use std::iter::Map;
use std::str::Chars;

// In terms of the return type, I don't really understand this.
// I've let the compiler error guide me.
fn student_plants_getter(
    student: &str,
) -> impl FnMut(&str) -> Map<Chars<'_>, fn(char) -> &'static str> {
    let pos = STUDENTS
        .binary_search(&student)
        .unwrap_or_else(|_| panic!("Student {} not found", student));
    move |row: &str| row[pos * 2..(pos + 1) * 2].chars().map(get_plant_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Student Lucreția not found")]
    fn student_plants_getter_panics_for_unknown_student() {
        let _ = student_plants_getter("Lucreția");
    }
}
