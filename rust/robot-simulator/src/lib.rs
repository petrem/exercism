mod arrow;
mod matrix;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

use crate::arrow::Arrow;
use crate::matrix::Matrix;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Robot {
    pos: Arrow<i32>,
    d: Direction, // because we are forced to return a reference to Direction
    dir: Arrow<i32>,
}

const LEFT_TURN: [[i32; 2]; 2] = [[0, -1], [1, 0]];
const RIGHT_TURN: [[i32; 2]; 2] = [[0, 1], [-1, 0]];

impl Robot {
    pub fn new(x: i32, y: i32, d: Direction) -> Self {
        Self {
            pos: Arrow::new(x, y),
            d,
            dir: Direction::into(d),
        }
    }

    #[must_use]
    pub fn turn_right(self) -> Self {
        self.turn(RIGHT_TURN.into())
    }

    #[must_use]
    pub fn turn_left(self) -> Self {
        self.turn(LEFT_TURN.into())
    }

    fn turn(self, rot_matrix: Matrix<i32>) -> Self {
        let dir = self.dir.rotate(rot_matrix);
        let d = Direction::try_from(&dir)
            .map_err(|err| format!("Could not create new Direction: {err}"))
            .unwrap();

        Self { dir, d, ..self }
    }

    #[must_use]
    pub fn advance(self) -> Self {
        Self {
            pos: self.pos.translate(&self.dir),
            ..self
        }
    }

    #[must_use]
    pub fn instructions(self, instructions: &str) -> Self {
        instructions
            .chars()
            .fold(self, |robot, instruction| match instruction {
                'L' => robot.turn_left(),
                'R' => robot.turn_right(),
                'A' => robot.advance(),
                c => panic!("Unknown instruction: {c}"),
            })
    }

    pub fn position(&self) -> (i32, i32) {
        (self.pos.x, self.pos.y)
    }

    pub fn direction(&self) -> &Direction {
        &self.d
    }
}

impl TryFrom<&Arrow<i32>> for Direction {
    type Error = String;

    fn try_from(value: &Arrow<i32>) -> Result<Self, Self::Error> {
        match (value.x.signum(), value.y.signum()) {
            (0, 1) => Ok(Direction::North),
            (1, 0) => Ok(Direction::East),
            (0, -1) => Ok(Direction::South),
            (-1, 0) => Ok(Direction::West),
            _ => Err(format!("Cannot determine direction from {:?}", value)),
        }
    }
}

impl From<Direction> for Arrow<i32> {
    fn from(value: Direction) -> Self {
        match value {
            Direction::North => Arrow::new(0, 1),
            Direction::East => Arrow::new(1, 0),
            Direction::South => Arrow::new(0, -1),
            Direction::West => Arrow::new(-1, 0),
        }
    }
}
