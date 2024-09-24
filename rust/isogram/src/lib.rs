pub fn check(candidate: &str) -> bool {
    let mut found_letter = [false; 26];
    candidate
        .chars()
        .find_map(|c| {
            c.is_ascii_alphabetic()
                .then(|| {
                    let letter_index = c.to_ascii_lowercase() as usize - 97;
                    match found_letter[letter_index] {
                        false => {
                            found_letter[letter_index] = true;
                            None
                        }
                        true => Some(()),
                    }
                })
                .flatten()
        })
        .is_none()
}
