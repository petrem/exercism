pub fn reverse(input: &str) -> String {
    fn rev(input: &str, mut acc: String) -> String {
        let mut input_chars = input.chars();
        match input_chars.next() {
            None => acc,
            Some(head) => {
                acc.insert(0, head);
                rev(input_chars.as_str(), acc)
            }
        }
    }

    rev(input, String::new())
}
