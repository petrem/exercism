use std::ops::{AddAssign, Deref, Index, IndexMut};

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    let mut minefield = Minefield::try_from(minefield).unwrap();
    minefield.annotate();
    dbg!(minefield).into()
}

#[derive(Debug)]
struct Minefield {
    cells: Vec<Vec<Cell>>,
    rows: usize,
    cols: usize,
}

impl Minefield {
    fn annotate(&mut self) {
        for row in 0..self.rows {
            for col in 0..self.cols {
                if let Cell::Mine = self[(row, col)] {
                    self.update_surrounding_cells(&row, &col);
                }
            }
        }
    }

    #[rustfmt::skip]
    const DELTAS: [(isize, isize); 8] = [
        (-1, -1), (-1, 0), (-1, 1),
        ( 0, -1),          ( 0, 1),
        ( 1, -1), ( 1, 0), ( 1, 1)
    ];

    fn update_surrounding_cells(&mut self, row: &usize, col: &usize) {
        let rows = self.rows as isize;
        let cols = self.cols as isize;
        println!("at ({row}, {col})");
        Minefield::DELTAS
            .iter()
            .map(|(delta_row, delta_col)| (*row as isize + delta_row, *col as isize + delta_col))
            .filter(|(r, c)| 0 <= *r && *r < rows && 0 <= *c && *c < cols)
            .for_each(|(r, c)| {
                self[(r as usize, c as usize)] += 1;
                println!("mut {r},{c}");
            });
    }
}

impl TryFrom<&[&str]> for Minefield {
    type Error = &'static str;

    fn try_from(minefield: &[&str]) -> Result<Self, Self::Error> {
        let rows = minefield.len();
        if let Some(first_row) = minefield.first() {
            let cols = first_row.len();
            let mut cells: Vec<Vec<Cell>> = Vec::with_capacity(rows);
            for colstr in minefield.iter() {
                let mut col = Vec::with_capacity(cols);
                if colstr.len() != cols {
                    return Err("Some columns have different lengths.");
                }
                colstr
                    .chars()
                    .try_for_each(|c| -> Result<(), &'static str> {
                        col.push(match c {
                            ' ' => Ok(Cell::Clean(0)),
                            '*' => Ok(Cell::Mine),
                            _ => Err("Found invalid character."),
                        }?);
                        Ok(())
                    })?;
                cells.push(col);
            }
            Ok(Minefield { cells, rows, cols })
        } else {
            // No rows
            Ok(Self {
                cells: vec![],
                rows: 0,
                cols: 0,
            })
        }
    }
}

impl From<Minefield> for Vec<String> {
    fn from(val: Minefield) -> Self {
        val.cells
            .iter()
            .map(|row| {
                row.iter()
                    .map(|cell| match cell {
                        Cell::Mine => "*".to_string(),
                        Cell::Clean(0) => " ".to_string(),
                        Cell::Clean(mines) => mines.to_string(),
                    })
                    .collect()
            })
            .collect()
    }
}

impl Deref for Minefield {
    type Target = Vec<Vec<Cell>>;

    fn deref(&self) -> &Self::Target {
        &self.cells
    }
}

impl Index<(usize, usize)> for Minefield {
    type Output = Cell;

    fn index(&self, (row, col): (usize, usize)) -> &Self::Output {
        &self.cells[row][col]
    }
}

impl IndexMut<(usize, usize)> for Minefield {
    fn index_mut(&mut self, (row, col): (usize, usize)) -> &mut Self::Output {
        &mut self.cells[row][col]
    }
}

#[derive(Debug)]
enum Cell {
    Clean(u8),
    Mine,
}

impl AddAssign<u8> for Cell {
    fn add_assign(&mut self, rhs: u8) {
        if let Cell::Clean(val) = self {
            *self = Cell::Clean(*val + rhs);
        }
    }
}
