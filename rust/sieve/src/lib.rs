pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    if upper_bound < 2 {
        return vec![];
    }

    let mut numbers: Vec<u64> = Vec::with_capacity((upper_bound.saturating_sub(2)) as usize);
    let check_bound = (upper_bound as f64).sqrt() as usize;

    numbers.extend(2..=upper_bound);

    for i in 2..=check_bound {
        if numbers[i - 2] != 0 {
            for j in (i * i..=(upper_bound as usize)).step_by(i) {
                numbers[j - 2] = 0;
            }
        }
    }

    numbers.retain(|x| *x != 0);
    numbers.shrink_to_fit();
    numbers
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore]
    fn find_primes_up_to_100_000_000() {
        let output = primes_up_to(100_000_000);
        assert_eq!(output.len(), 5761455); // from https://en.wikipedia.org/wiki/Prime_number_theorem#Table_of_%CF%80(x),_x_/_log_x,_and_li(x)
    }
}
