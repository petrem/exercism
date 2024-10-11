#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    InvalidInputBase,
    InvalidOutputBase,
    InvalidDigit(u32),
}

///
/// Convert a number between two bases.
///
/// A number is any slice of digits.
/// A digit is any unsigned integer (e.g. u8, u16, u32, u64, or usize).
/// Bases are specified as unsigned integers.
///
/// Return the corresponding Error enum if the conversion is impossible.
///
///
/// You are allowed to change the function signature as long as all test still pass.
///
///
/// Example:
/// Input
///   number: &[4, 2]
///   from_base: 10
///   to_base: 2
/// Result
///   Ok(vec![1, 0, 1, 0, 1, 0])
///
/// The example corresponds to converting the number 42 from decimal
/// which is equivalent to 101010 in binary.
///
///
/// Notes:
///  * The empty slice ( "[]" ) is equal to the number 0.
///  * Never output leading 0 digits, unless the input number is 0, in which the output must be `[0]`.
///    However, your function must be able to process input with leading 0 digits.
///
pub fn convert(number: &[u32], from_base: u32, to_base: u32) -> Result<Vec<u32>, Error> {
    _ = from_base < 2 && Err(Error::InvalidInputBase)?;
    _ = to_base < 2 && Err(Error::InvalidOutputBase)?;
    let n = number.iter().try_fold(0, |acc, &elt| {
        (elt < from_base)
            .then_some(acc * from_base + elt)
            .ok_or(Error::InvalidDigit(elt))
    })?;
    Ok((n == 0).then_some(vec![0]).unwrap_or(from_u32(to_base, n)))
}

fn from_u32(base: u32, value: u32) -> Vec<u32> {
    let mut digits: Vec<_> = std::iter::successors(
        (value > 0).then_some((value / base, value % base)),
        |(value, _)| (*value > 0).then_some((value / base, value % base)),
    )
    .map(|(_, d)| d)
    .collect();
    digits.reverse();
    digits
}
