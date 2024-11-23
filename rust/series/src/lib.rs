pub fn series(digits: &str, len: usize) -> Vec<String> {
    (0..(digits.len() + 1).saturating_sub(len)).map(|start| digits[start..(start+len).min(digits.len())].to_string()).collect()
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_0_length() {
        let input = "123";
        let length = 0;
        let output = series(input, length);
        let expected = &["", "", "", ""];
        assert_eq!(output, expected);
    }
}
*/
