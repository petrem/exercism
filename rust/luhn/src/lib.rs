/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    code.chars().filter(|c| *c != ' ').map(|c| c.to_digit(10)).rev()
        .collect::<Option<Vec<_>>>().and_then(|ds| (ds.len() >= 2).then_some(ds))
        .map(|ds| {
            ds.iter().zip([false, true].iter().cycle()).fold(0, |acc, (d, &double)| {
                acc + if double { [0, 2, 4, 6, 8, 1, 3, 5, 7, 9][*d as usize] } else { *d }
            }) % 10 == 0
        }).unwrap_or(false)
}
