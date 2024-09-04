pub fn reverse(input: &str) -> String {
    let mut reversed = String::new();
    for chr in input.chars().rev() {
        reversed.push(chr);
    }
    reversed
}

// TODO:
// 1 - https://crates.io/crates/unicode-segmentation
// 2 - collect()
