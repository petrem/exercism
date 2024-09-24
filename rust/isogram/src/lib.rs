use std::collections::HashSet;

pub fn check(candidate: &str) -> bool {
    let mut letters: HashSet<char> = ('a'..='z').collect();
    candidate
        .chars()
        .filter(char::is_ascii_alphabetic)
        .map(|c| c.to_ascii_lowercase())
        .find_map(|c| (!letters.remove(&c)).then_some(()))
        .is_none()
}
