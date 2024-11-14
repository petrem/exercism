use std::iter::{repeat, successors};

pub fn factors(n: u64) -> Vec<u64> {
    let mut factors: Vec<_> = Primes::new()
        .take_while(|&p| p * p <= n)
        .flat_map(|p| {
            repeat(p).take(successors(Some(n), |r| (r % p == 0).then_some(r / p)).count() - 1)
        })
        .collect();
    let product: u64 = factors.iter().product();
    if n / product != 1 {
        factors.push(n / product);
    }
    factors
}

struct Primes {
    primes: Vec<u64>,
}

impl Primes {
    fn new() -> Self {
        Self { primes: vec![] }
    }
}

impl Iterator for Primes {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        match self.primes.len() {
            0 => {
                self.primes.push(2);
            }
            1 => {
                self.primes.push(3);
            }
            _ => {
                if let Some(previous) = self.primes.last() {
                    for candidate in ((*previous + 2)..).step_by(2) {
                        if is_pairwise_coprime_with(candidate, &self.primes) {
                            self.primes.push(candidate);
                            break;
                        }
                    }
                };
            }
        };
        Some(*self.primes.last().unwrap())
    }
}

fn is_pairwise_coprime_with(n: u64, ps: &[u64]) -> bool {
    !ps.iter().take_while(|&p| p * p <= n).any(|&p| n % p == 0)
}

#[cfg(test)]
mod tests {

    use super::*;
    // A small composite with one prime factor larger than
    // its square.
    #[test]
    fn product_of_primes() {
        let factors = factors(38);
        let expected = [2, 19];
        assert_eq!(factors, expected);
    }
}
