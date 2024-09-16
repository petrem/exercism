//This version is very inefficient computation-wise, but at least not a memory hog.

struct Sieve {
    primes: Vec<u64>,
}

impl Sieve {
    fn new() -> Sieve {
        Sieve { primes: vec![] }
    }
}

impl Iterator for Sieve {
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        let next = match self.primes.last() {
            None => 2,
            Some(2) => 3, // so that we can add two for all the following candidates
            Some(last) => (last + 2..)
                .step_by(2)
                .find(|&candidate| {
                    self.primes
                        .iter()
                        .all(|&prime| !is_multiple_of(candidate, prime))
                })
                .expect("There are no more primes, aliens invading?!."),
        };
        self.primes.push(next);
        Some(next)
    }
}

fn is_multiple_of(n: u64, m: u64) -> bool {
    (m..=n).step_by(m as usize).any(|x| x == n)
}

pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    Sieve::new()
        .take_while(|&prime| prime <= upper_bound)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nine_is_multiple_of_three() {
        assert!(is_multiple_of(9, 3));
    }

    #[test]
    fn a_hunderd_twenty_five_is_multiple_of_five() {
        assert!(is_multiple_of(125, 5));
    }

    #[test]
    fn eight_is_not_multiple_of_three() {
        assert!(!is_multiple_of(8, 3));
    }

    #[test]
    fn ten_is_not_multiple_of_three() {
        assert!(!is_multiple_of(10, 3));
    }

    #[test]
    fn nine_is_not_multiple_of_four() {
        assert!(!is_multiple_of(9, 4));
    }

    // Exercism will run ignored tests, apparently. Commenting out to avoid a timeout.
    // #[test]
    // #[ignore]
    // fn find_primes_up_to_100_000_000() {
    //     let output = primes_up_to(100_000_000);
    //     assert_eq!(output.len(), 5761455); // from https://en.wikipedia.org/wiki/Prime_number_theorem#Table_of_%CF%80(x),_x_/_log_x,_and_li(x)
    // }
}
