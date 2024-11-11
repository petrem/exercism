/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    let mut digits = vec![];
    for c in code.chars().filter(|c| *c != ' ').rev() {
        if let Some(d) = c.to_digit(10) {
            digits.push(d);
        } else {
            return false;
        }
    }
    if digits.len() < 2 {
        return false;
    }
    let luhn_sum_odds: u32 = digits
        .iter()
        .skip(1)
        .step_by(2)
        .map(|d| if d * 2 < 10 { d * 2 } else { d * 2 - 9 })
        .sum();
    let luhn_sum_evens: u32 = digits.iter().step_by(2).sum();
    (luhn_sum_odds + luhn_sum_evens) % 10 == 0
}
