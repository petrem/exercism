use std::iter::repeat;

pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    let mut markers: Vec<bool> = repeat(true).take((upper_bound + 1) as usize).collect();
    markers[0] = false;
    markers[1] = false;

    let check_bound = (upper_bound as f64).sqrt() as usize;

    for i in 2..=check_bound {
        if markers[i] {
            for j in (i * i..=(upper_bound as usize)).step_by(i) {
                markers[j] = false;
            }
        }
    }
    markers
        .into_iter()
        .enumerate()
        .filter(|(_, b)| *b)
        .map(|(k, _)| k as u64)
        .collect()
}

// exercism will run ignored tests, apparently -- so it times out

// #[cfg(test)]
// mod tests {
//     use super::*;
//     #[test]
//     #[ignore]
//     fn find_primes_up_to_100_000_000() {
//         let output = primes_up_to(100_000_000);
//         assert_eq!(output.len(), 5761455); // from https://en.wikipedia.org/wiki/Prime_number_theorem#Table_of_%CF%80(x),_x_/_log_x,_and_li(x)
//     }
// }
