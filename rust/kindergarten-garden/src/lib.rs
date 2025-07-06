//! Kindergarten Garden::Version 2::Goodbye vectors, hello iterators.

/// Determine the plants belonging to `student` given a `diagram`.
pub fn plants(diagram: &str, student: &str) -> Vec<&'static str> {
    diagram
        .lines()
        .flat_map(student_plants_mapper(student, &get_plant_name))
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

// I learned this way of typing the return type of this function from my mentor.
// Thank you!
fn student_plants_mapper<'a, F>(
    student: &str,
    f: &'a F,
) -> impl FnMut(&'a str) -> Box<dyn Iterator<Item = &'static str> + 'a>
where
    F: Fn(char) -> &'static str + 'a,
{
    let pos = STUDENTS
        .binary_search(&student)
        .unwrap_or_else(|_| panic!("Student {student} not found"));
    move |row: &str| Box::new(row[pos * 2..(pos + 1) * 2].chars().map(f))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn student_plants_mapper_get_plants_for_existing_student() {
        let mut ileana_getter = student_plants_mapper("Ileana", &get_plant_name);
        let plants_row = "RRRRRRRRRRRRRRRRCVRRRRRR";
        let ileanas_plants: Vec<&str> = ileana_getter(&plants_row).collect();
        let expected = vec!["clover", "violets"];
        assert_eq!(expected, ileanas_plants);
    }

    #[test]
    #[should_panic(expected = "Student Lucreția not found")]
    fn student_plants_getter_panics_for_unknown_student() {
        let _ = student_plants_mapper("Lucreția", &|_| "not used");
    }
}
