pub fn series(digits: &str, len: usize) -> Vec<String> {
    (0..(digits.len() + 1).saturating_sub(len)).map(|start| digits[start..(start+len).min(digits.len())].to_string()).collect()
}

