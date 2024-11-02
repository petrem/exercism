use std::{
    iter::repeat,
    ops::{AddAssign, Deref, Index, IndexMut},
};

pub fn spiral_matrix(size: u32) -> Vec<Vec<u32>> {
    let mut spiral = Spiral::new(size as usize);
    spiral.build();
    spiral.to_vec()
}

struct Spiral {
    data: Vec<Vec<u32>>,
    size: usize,
}

impl Spiral {
    fn new(size: usize) -> Self {
        Self {
            data: repeat(vec![0; size]).take(size).collect(),
            size,
        }
    }

    fn build(&mut self) {
        let mut cursor = Pair(0, -1);
        for (value, direction) in self.walk_spiral().enumerate() {
            cursor += direction;
            self[cursor] = 1 + value as u32;
        }
    }

    const RIGHT: Pair = Pair(0, 1);
    const DOWN: Pair = Pair(1, 0);
    const LEFT: Pair = Pair(0, -1);
    const UP: Pair = Pair(-1, 0);

    fn walk_spiral(&mut self) -> impl Iterator<Item = Pair> {
        (1..self.size)
            .rev()
            .step_by(2)
            .flat_map(|k| {
                (repeat(Spiral::RIGHT).take(k + 1))
                    .chain(repeat(Spiral::DOWN).take(k))
                    .chain(repeat(Spiral::LEFT).take(k))
                    .chain(repeat(Spiral::UP).take(k - 1))
            })
            .chain(repeat(Spiral::RIGHT).take(self.size % 2))
    }
}

impl Deref for Spiral {
    type Target = Vec<Vec<u32>>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

#[derive(Clone, Copy, Debug)]
struct Pair(isize, isize);
impl AddAssign for Pair {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}

impl Index<Pair> for Spiral {
    type Output = u32;

    fn index(&self, index: Pair) -> &Self::Output {
        &self.data[index.0 as usize][index.1 as usize]
    }
}
impl IndexMut<Pair> for Spiral {
    fn index_mut(&mut self, index: Pair) -> &mut Self::Output {
        &mut self.data[index.0 as usize][index.1 as usize]
    }
}
