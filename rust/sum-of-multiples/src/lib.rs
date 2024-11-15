use std::collections::HashSet;

pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    factors
        .iter()
        .filter(|&&m| m != 0)
        .flat_map(|&m| (1..(limit.div_ceil(m))).map(move |i| i * m))
        .collect::<HashSet<_>>()
        .iter()
        .sum()
}
