use std::iter::{once, successors, zip};

pub struct PascalsTriangle {
    row_count: usize,
}

impl PascalsTriangle {
    pub fn new(row_count: u32) -> Self {
        Self {
            row_count: row_count as usize,
        }
    }

    pub fn rows(&self) -> Vec<Vec<u32>> {
        if self.row_count == 0 {
            return vec![];
        }
        successors(Some(vec![1]), |prev| Some(next_row(prev)))
            .take(self.row_count)
            .collect()
    }
}

fn next_row(prev: &[u32]) -> Vec<u32> {
    zip(once(&0).chain(prev.iter()), prev.iter().chain(once(&0)))
        .map(|(a, b)| a + b)
        .collect()
}
