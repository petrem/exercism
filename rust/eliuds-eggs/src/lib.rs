pub fn egg_count(display_value: u32) -> usize {
    std::iter::successors(Some(display_value), |n| (*n > 0).then_some(std::ops::Shr::shr(n, 1))).filter(|n| n & 1 == 1).count()
}
