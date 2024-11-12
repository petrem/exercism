pub fn is_armstrong_number(num: u32) -> bool {
    let digits = digits_from(num);
    let n_digits = digits.len() as u32;
    digits_from(num)
        .iter()
        .map(|d| d.pow(n_digits))
        .sum::<u32>()
        == num
}

fn digits_from(num: u32) -> Vec<u32> {
    std::iter::successors(Some((num, 0)), |(n, _)| {
        (*n != 0).then_some((n / 10, n % 10))
    })
    .skip(1)
    .map(|(_, r)| r)
    .collect()
}
