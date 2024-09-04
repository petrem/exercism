const STUDENTS: [&str; 12] = [
    "Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph",
    "Kincaid", "Larry",
];

fn student_index(student: &str) -> usize {
    // We're allowed to panic because we can't handle errors in `plants()`.
    STUDENTS.iter().position(|&x| x == student).unwrap()
}

fn plant(repr: char) -> &'static str {
    match repr {
        'C' => "clover",
        'G' => "grass",
        'R' => "radishes",
        'V' => "violets",
        _ => panic!("Uh oh, unknown plant, could be poisonous!"),
    }
}

fn student_plants_on_row(row: &str, student: &str) -> Vec<&'static str> {
    let pos = student_index(student);
    row[pos * 2..(pos + 1) * 2].chars().map(plant).collect()
}

pub fn plants(diagram: &str, student: &str) -> Vec<&'static str> {
    let rows: Vec<&str> = diagram.lines().collect();
    rows.iter()
        .flat_map(|row| student_plants_on_row(row, student))
        .collect()
}
