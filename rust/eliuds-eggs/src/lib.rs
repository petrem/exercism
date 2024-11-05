pub fn egg_count(display_value: u32) -> usize {
    let mut count: usize = 0;
    let mut eggs = display_value as usize;
    while eggs > 0 {
        count += eggs & 1;
        eggs >>= 1;
    }
    count
}
