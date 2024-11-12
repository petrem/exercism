/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    if let Some(digits) = code
        .chars()
        .filter(|c| *c != ' ')
        .map(|c| c.to_digit(10))
        .rev()
        .collect::<Option<Vec<_>>>()
        .and_then(|ds| (ds.len() >= 2).then_some(ds)) {
            let luhn_sum_odds: u32 = digits.iter().skip(1).step_by(2).map(|d| if d * 2 < 10 {d * 2} else {d * 2 - 9}).sum();
            let luhn_sum_evens: u32 = digits.iter().step_by(2).sum();
            (luhn_sum_odds + luhn_sum_evens) % 10 == 0
        } else {
            false
        }
}

