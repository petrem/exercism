pub fn rotate(input: &str, key: u8) -> String {
    input
        .chars()
        .map(|c| match () {
            _ if c.is_ascii_uppercase() => ((c as u8 + key - 13) % 26 + 65) as char,
            _ if c.is_ascii_lowercase() => ((c as u8 + key - 19) % 26 + 97) as char,
            _ => c,
        })
        .collect()
}
