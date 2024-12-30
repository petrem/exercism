/*
Shamefully adapted from https://exercism.org/tracks/rust/exercises/parallel-letter-frequency/solutions/rsalmei
See also https://doc.rust-lang.org/std/thread/fn.scope.html
*/

use std::collections::HashMap;
use std::thread;

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    // redirect to the best implementation (that is, small inputs don't get parallelized)
    match input.len() {
        0 => HashMap::new(),
        n if n < 500 => count_letters(input),
        _ => thread::scope(|s| {
            let mut handles = Vec::with_capacity(worker_count);
            for lines in input.chunks(input.len() / worker_count + 1) {
                handles.push(s.spawn(|| count_letters(lines)))
            }

            let mut map = handles.pop().unwrap().join().unwrap();
            for res in handles {
                res.join().unwrap().into_iter().for_each(|(k, v)| {
                    *map.entry(k).or_default() += v;
                })
            }

            map
        }),
    }
}

fn count_letters(input: &[&str]) -> HashMap<char, usize> {
    let mut counts = HashMap::new();
    for line in input {
        for c in line
            .chars()
            .filter(|c| c.is_alphabetic())
            .map(|c| c.to_ascii_lowercase())
        {
            *counts.entry(c).or_default() += 1;
        }
    }
    counts
}
