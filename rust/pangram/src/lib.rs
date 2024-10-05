use std::collections::HashSet;

/// Determine whether a sentence is a pangram.
pub fn is_pangram(sentence: &str) -> bool {
    HashSet::from_iter('a'..='z')
        .is_subset(&HashSet::<char>::from_iter(sentence.to_lowercase().chars()))
}
