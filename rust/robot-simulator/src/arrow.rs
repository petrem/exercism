//! Simple, two-dimensional vectors.
//! The term "arrow" is vague, but hopefully less misleading than Vector,
//! which could be confused with either a `Vec` or a multi-dimensional vector. I guess.

use crate::matrix::Matrix;
use std::{
    iter::Sum,
    ops::{Add, AddAssign, Mul},
};

/// A two-dimentional vector
// (could we say, over a field T? we dont' need it to be a field in general..)
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Arrow<T> {
    pub x: T,
    pub y: T,
}

impl<T> Add for Arrow<T>
where
    T: Add<Output = T> + Clone,
{
    type Output = Self;

    fn add(self, other: Arrow<T>) -> Self::Output {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl<T, Scalar> Mul<Scalar> for Arrow<T>
where
    T: Mul<Scalar, Output = T>,
    Scalar: Copy,
{
    type Output = Self;

    fn mul(self, other: Scalar) -> Self::Output {
        Self {
            x: self.x * other,
            y: self.y * other,
        }
    }
}

impl<T> Arrow<T> {
    pub fn new(x: T, y: T) -> Arrow<T>
    where
        T: Clone,
    {
        Arrow { x, y }
    } //{x: x.clone(), y: y.clone()} }

    pub fn translate(self, other: &Arrow<T>) -> Arrow<T>
    where
        T: Add<Output = T> + Clone,
    {
        Arrow::new(self.x + other.x.clone(), self.y + other.y.clone())
    }

    pub fn translate_mut(&mut self, other: &Arrow<T>)
    where
        T: AddAssign + Clone,
    {
        self.x += other.x.clone();
        self.y += other.y.clone();
    }

    pub fn rotate(self, rot_matrix: Matrix<T>) -> Arrow<T>
    where
        T: Clone + Sum + Mul<Output = T>,
    {
        let rotated = rot_matrix * Matrix::from(self).transpose();
        Arrow::try_from(rotated).unwrap()
    }
}

impl<T> TryFrom<Matrix<T>> for Arrow<T>
where
    T: Clone,
{
    type Error = &'static str;

    fn try_from(value: Matrix<T>) -> Result<Self, Self::Error> {
        match (value.n_rows(), value.n_cols()) {
            (1, 2) => Ok(value.row(0)),
            (2, 1) => Ok(value.col(0)),
            _ => Err("Cannot create arrow from matrix of more than two cells."),
        }
        .map(|coords| Arrow::new(coords[0].clone(), coords[1].clone()))
    }
}

impl<T> From<Arrow<T>> for Matrix<T>
where
    T: Clone,
{
    /// Convert arrow into a 1×2 matrix
    fn from(value: Arrow<T>) -> Self {
        Matrix::from([[value.x, value.y]])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arrow_translate() {
        let position = Arrow::new(1, 2);
        let translation = Arrow::new(3, 4);
        let expected_position = Arrow::new(4, 6);
        assert_eq!(expected_position, position.translate(&translation));
    }

    #[test]
    fn arrow_rotate_left() {
        let direction_right = Arrow::new(1, 0);
        let turn_left = Matrix::from([[0, -1], [1, 0]]);
        let direction_up = Arrow::new(0, 1);
        assert_eq!(direction_up, direction_right.rotate(turn_left));
    }

    #[test]
    fn arrow_try_from_invalid_matrix() {
        let matrix = Matrix::from([[1, 2, 3]]);
        let maybe_arrow = Arrow::try_from(matrix);
        assert!(maybe_arrow.is_err());
    }
}
