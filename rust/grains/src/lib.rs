//Every time I encounter this, I read "Braaaaains!!"

pub fn square(s: u32) -> u64 {
    if s < 1 {
        panic!("No Braaaains?!?!?!")
    }
    2u64.pow(s - 1)
}

pub fn total() -> u64 {
    // This is 2^64 - 1, but let's entertain it...
    (1..=64).map(square).sum()
}
