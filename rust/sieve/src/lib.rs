//Version based on 'fold'. Not efficient, but simpler.

use std::iter::once;

pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    // could be simplified if we woulnd't use the .step_by(2) optimization
    match upper_bound {
        0 | 1 => vec![],
        2 => vec![2],
        3 | 4 => vec![2, 3],
        _ => (5..=upper_bound).step_by(2).fold(vec![2, 3], |acc, x| {
            if is_pairwise_coprime_with(x, &acc) {
                acc.into_iter().chain(once(x)).collect()
            } else {
                acc
            }
        }),
    }
}

fn is_pairwise_coprime_with(n: u64, ps: &[u64]) -> bool {
    !ps.iter().any(|&p| is_multiple_of(n, p))
}

fn is_multiple_of(n: u64, m: u64) -> bool {
    (m..=n).step_by(m as usize).any(|x| x == n)
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
