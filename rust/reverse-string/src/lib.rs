pub fn reverse(input: &str) -> String {
    let mut reversed = String::new();
    for chr in input.chars().rev() {
        reversed.push(chr);
    }
    reversed
}
