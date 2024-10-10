use std::{
    iter::{repeat, zip, Sum},
    ops::{Add, Mul},
};

/// A matrix of n×m elements of type `T` where n is the number of rows
/// and m is the number of columns.
#[derive(Debug, Clone)]
pub struct Matrix<T> {
    n: usize,
    m: usize,
    matrix: Vec<Vec<T>>,
}

impl<T> Matrix<T> {
    pub fn new(n: usize, m: usize, fill: T) -> Matrix<T>
    where
        T: Clone,
    {
        let row = Vec::from_iter(repeat(fill).take(m));
        Matrix {
            n,
            m,
            matrix: Vec::from_iter(repeat(row).take(n)),
        }
    }

    pub fn n_rows(&self) -> usize {
        self.n
    }
    pub fn n_cols(&self) -> usize {
        self.m
    }

    pub fn row(&self, index: usize) -> Vec<T>
    where
        T: Clone,
    {
        if index >= self.n {
            panic!("Cannot get row with index past the last.");
        }
        self.matrix[index].clone()
    }

    pub fn col(&self, index: usize) -> Vec<T>
    where
        T: Clone,
    {
        if index >= self.m {
            panic!("Cannot get column with index past the last.");
        }
        self.rows().map(|row| row[index].clone()).collect()
    }

    pub fn rows(&self) -> MatrixRows<T> {
        MatrixRows::new(self)
    }

    pub fn cols(&self) -> MatrixCols<T> {
        MatrixCols::new(self)
    }

    pub fn transpose(&self) -> Matrix<T>
    where
        T: Clone,
    {
        let mut matrix = Vec::from_iter(repeat(vec![]).take(self.m));
        for row in self.rows() {
            for (j, elt) in row.iter().enumerate() {
                matrix[j].push(elt.clone());
            }
        }

        Matrix {
            n: self.m,
            m: self.n,
            matrix,
        }
    }
}

pub struct MatrixRows<'a, T> {
    matrix: &'a Matrix<T>,
    current: usize,
}

impl<T> MatrixRows<'_, T> {
    fn new(matrix: &Matrix<T>) -> MatrixRows<T> {
        MatrixRows { current: 0, matrix }
    }
}

impl<'a, T> Iterator for MatrixRows<'a, T> {
    type Item = &'a Vec<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current >= self.matrix.n {
            return None;
        }
        let current = self.current;
        self.current += 1;

        Some(&self.matrix.matrix[current])
    }
}

pub struct MatrixCols<'a, T> {
    matrix: &'a Matrix<T>,
    current: usize,
}

impl<T> MatrixCols<'_, T> {
    fn new(matrix: &Matrix<T>) -> MatrixCols<T> {
        MatrixCols { current: 0, matrix }
    }
}

impl<T: Clone> Iterator for MatrixCols<'_, T> {
    type Item = Vec<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current >= self.matrix.m {
            return None;
        }
        let current = self.current;
        self.current += 1;

        Some(self.matrix.col(current))
    }
}

impl<'a, T> IntoIterator for &'a Matrix<T> {
    type Item = &'a Vec<T>;
    type IntoIter = MatrixRows<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.rows()
    }
}

impl<T> Add for Matrix<T>
where
    T: Add<Output = T> + Clone,
{
    type Output = Self;

    fn add(self, other: Matrix<T>) -> Self::Output {
        if self.n != other.n || self.m != other.m {
            panic!("Cannot add matrices of different dimensions.");
        }
        Matrix {
            matrix: zip(&self, &other)
                .map(|(row_a, row_b)| {
                    zip(row_a, row_b)
                        .map(|(elt_a, elt_b)| elt_a.clone() + elt_b.clone())
                        .collect()
                })
                .collect(),
            ..self
        }
    }
}

impl<T> Mul for Matrix<T>
where
    T: Sum + Mul<Output = T> + Clone,
{
    type Output = Self;

    fn mul(self, other: Matrix<T>) -> Self::Output {
        if self.m != other.n {
            panic!("Cannot multiply matrices where first #cols differes from second #rows.");
        }
        Matrix {
            matrix: self
                .rows()
                .map(|row| {
                    other
                        .cols()
                        .map(|col| zip(row, col).map(|(a, b)| a.clone() * b).sum())
                        .collect()
                })
                .collect(),
            n: self.n,
            m: other.m,
        }
    }
}

impl<Scalar> Mul<Scalar> for Matrix<Scalar>
where
    Scalar: Mul<Output = Scalar> + Clone,
{
    type Output = Self;

    fn mul(self, scalar: Scalar) -> Self::Output {
        Matrix {
            matrix: self
                .rows()
                .map(|row| row.iter().map(|elt| elt.clone() * scalar.clone()).collect())
                .collect(),
            ..self
        }
    }
}

impl<T: Clone, const N: usize, const M: usize> From<[[T; M]; N]> for Matrix<T> {
    fn from(matrix: [[T; M]; N]) -> Matrix<T> {
        Matrix {
            n: N,
            m: M,
            matrix: matrix.iter().map(|row| row.to_vec()).collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matrix_test_new() {
        let n = 3;
        let m = 4;
        let matrix = Matrix::new(n, m, 3);
        let expected = vec![vec![3, 3, 3, 3], vec![3, 3, 3, 3], vec![3, 3, 3, 3]];
        assert_eq!(n, matrix.n);
        assert_eq!(m, matrix.m);
        assert_eq!(expected, matrix.matrix);
    }

    #[test]
    fn matrix_test_rows_count() {
        let matrix = Matrix::new(3, 2, 'x');
        let rows = matrix.rows();
        assert_eq!(rows.count(), 3);
    }

    #[test]
    fn matrix_test_iterate_rows() {
        let matrix = Matrix::new(3, 2, 'x');
        let expected_row = vec!['x', 'x'];
        for row in &matrix {
            assert_eq!(expected_row, *row);
        }
    }

    #[test]
    fn matrix_test_cols_count() {
        let matrix = Matrix::new(3, 2, 'x');
        let cols = matrix.cols();
        assert_eq!(cols.count(), 2);
    }
    #[test]
    fn matrix_test_iterate_columns() {
        let matrix = Matrix::new(3, 2, 'x');
        let expected_col = vec!['x', 'x', 'x'];
        for col in matrix.cols() {
            assert_eq!(expected_col, col);
        }
    }

    #[test]
    fn matrix_test_get_row() {
        let matrix = Matrix::new(2, 2, 'x');
        let expected_row = vec!['x', 'x'];
        assert_eq!(expected_row, matrix.row(1));
    }

    #[test]
    fn matrix_test_get_col() {
        let matrix = Matrix::new(2, 2, 'x');
        let expected_col = vec!['x', 'x'];
        assert_eq!(expected_col, matrix.col(1));
    }

    #[test]
    fn matrix_test_transpose() {
        let matrix = Matrix::new(3, 2, 1);
        let expected = vec![vec![1, 1, 1], vec![1, 1, 1]];
        assert_eq!(expected, matrix.transpose().matrix);
    }

    #[test]
    fn matrix_test_add() {
        let matrix1 = Matrix::new(2, 3, 1);
        let matrix2 = Matrix::new(2, 3, 2);
        let expected = vec![vec![3, 3, 3], vec![3, 3, 3]];
        assert_eq!(expected, (matrix1 + matrix2).matrix);
    }

    #[test]
    fn matrix_test_mul_with_matrix() {
        let matrix1 = Matrix::new(2, 3, 2);
        let matrix2 = Matrix::new(3, 2, 3);
        let expected = vec![vec![18, 18], vec![18, 18]];
        assert_eq!(expected, (matrix1 * matrix2).matrix);
    }

    #[test]
    fn matrix_test_mul_with_scalar() {
        let matrix1 = Matrix::new(2, 3, 2);
        let scalar = 3;
        let expected = vec![vec![6, 6, 6], vec![6, 6, 6]];
        assert_eq!(expected, (matrix1 * scalar).matrix);
    }

    #[test]
    fn matrix_test_from_array() {
        let array = [[1, 2], [3, 4], [5, 6]];
        let matrix = Matrix::from(array);
        let expected = vec![vec![1, 2], vec![3, 4], vec![5, 6]];
        assert_eq!(3, matrix.n);
        assert_eq!(2, matrix.m);
        assert_eq!(expected, matrix.matrix);
    }

    /// struggling person's float approximate equality
    /// don't use this at home (or work!)
    fn approx_equal(a: f32, b: f32) -> bool {
        /// arbitrary value
        const MAX_DELTA: f32 = 0.0001;
        (a - b).abs() < MAX_DELTA
    }

    #[test]
    fn matrix_test_rotation_of_a_unit_vector() {
        use std::f32::consts::PI;
        let rot_angle = PI / 6.0;
        let rot_matrix: Matrix<f32> = Matrix::from([
            [rot_angle.cos(), -rot_angle.sin()],
            [rot_angle.sin(), rot_angle.cos()],
        ]);
        let vector_angle = PI / 3.0;
        let vector = Matrix::from([[vector_angle.cos()], [vector_angle.sin()]]);
        let expected_vector = Matrix::from([[0.0], [1.0]]);
        let result_vector = rot_matrix * vector;
        assert!(approx_equal(
            expected_vector.col(0)[0],
            result_vector.col(0)[0]
        ));
    }
}
