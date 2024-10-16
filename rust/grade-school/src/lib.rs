pub struct School<'a>(Vec<Grade<'a>>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Grade<'a> {
    grade: u32,
    roster: Vec<&'a str>,
}

impl<'a> School<'a> {
    pub fn new() -> School<'a> {
        School(vec![])
    }

    pub fn add(&mut self, grade: u32, student: &'a str) {
        if self.contains(student) {
            return;
        }
        let grade_idx = match self.grade_index(grade) {
            Ok(idx) => idx,
            Err(idx) => {
                self.0.insert(idx, Grade::new(grade));
                idx
            }
        };
        self.0[grade_idx].add(student);
    }

    pub fn grades(&self) -> Vec<u32> {
        self.0.iter().map(|grade| grade.grade).collect()
    }

    // If `grade` returned a reference, `School` would be forced to keep a `Vec<String>`
    // internally to lend out. By returning an owned vector of owned `String`s instead,
    // the internal structure can be completely arbitrary. The tradeoff is that some data
    // must be copied each time `grade` is called.
    pub fn grade(&self, grade: u32) -> Vec<String> {
        match self.grade_index(grade) {
            Ok(idx) => self.0[idx].roster.iter().map(|x| x.to_string()).collect(),
            Err(_) => vec![],
        }
    }

    fn grade_index(&self, grade: u32) -> Result<usize, usize> {
        self.0.binary_search_by_key(&grade, |g| g.grade)
    }

    fn contains(&self, student: &'a str) -> bool {
        self.0.iter().any(|g| g.contains(student))
    }
}

impl<'a> Grade<'a> {
    fn new(grade: u32) -> Grade<'a> {
        Grade {
            grade,
            roster: vec![],
        }
    }

    fn add(&mut self, student: &'a str) {
        if let Err(idx) = self.student_index(student) {
            self.roster.insert(idx, student);
        }
    }

    fn contains(&self, student: &'a str) -> bool {
        self.student_index(student).is_ok()
    }

    fn student_index(&self, student: &'a str) -> Result<usize, usize> {
        self.roster.binary_search_by(|&elt| (elt).cmp(student))
    }
}
