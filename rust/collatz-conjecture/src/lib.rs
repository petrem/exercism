//! Count hailstone steps to 1.
//! This version should be noticably faster than your run-of-the-mill solution.
//! Included benchmarking code, thanks to backdevjung's help.

pub fn collatz(n: u64) -> Option<u64> {
    match n {
        0 => None,
        1 => Some(0),
        _ => Some(collatz_optimized(n) as u64),
    }
}

fn collatz_optimized(n: u64) -> u32 {
    let (even_steps, odd_n) = if n % 2 == 0 { make_odd(n) } else { (0, n) };
    even_steps + hailstone_steps(odd_n)
}

/// Count hailstone steps. `n` MUST be odd and greater than 1!
fn hailstone_steps(n: u64) -> u32 {
    std::iter::successors(Some((0, n)), |&(_, r)| (r > 1).then_some(hailstone(r)))
        .fold(0, |steps, (s, _)| steps + s)
}

/// A hailstone version taking several steps at a time.
/// Its result is always odd, but its argument MUST be odd!
fn hailstone(n: u64) -> (u32, u64) {
    let (s, r) = make_odd(n * 3 + 1);
    (s + 1, r)
}

fn make_odd(n: u64) -> (u32, u64) {
    let tz = n.trailing_zeros();
    (tz, n >> tz)
}

// should be possible to dramatically improve with "standard"/Vermeulen polynomials
// see:
// @article{leavens19923x+,
//   title={3x+ 1 search programs},
//   author={Leavens, Gary T and Vermeulen, Mike},
//   journal={Computers \& Mathematics with Applications},
//   volume={24},
//   number={11},
//   pages={79--99},
//   year={1992},
//   publisher={Elsevier}
// }

pub fn run_of_the_mill_collatz(n: u64) -> Option<u64> {
    if n == 0 {
        return None;
    }
    let mut steps = 0;
    let mut r = n;
    while r > 1 {
        steps += 1;
        if r % 2 == 0 {
            r /= 2;
        } else {
            r = r * 3 + 1;
        }
    }
    Some(steps)
}
