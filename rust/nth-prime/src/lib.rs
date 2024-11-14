pub fn nth(n: u32) -> u32 {
    let mut primes = vec![2, 3];
    if n < 2 {
        return primes[n as usize];
    }
    for i in (5..).step_by(2) {
        if is_pairwise_coprime_with(i, &primes) {
            primes.push(i);
            if primes.len() == (n + 1) as usize {
                break;
            }
        }
    }
    *primes.last().unwrap()
}

fn is_pairwise_coprime_with(n: u32, ps: &[u32]) -> bool {
    !ps.iter().take_while(|&p| p * p <= n).any(|&p| n % p == 0)
}
