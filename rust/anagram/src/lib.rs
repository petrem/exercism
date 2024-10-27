use std::collections::HashSet;

// A bit better. Still, sorting is not really necessary.

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &'a [&str]) -> HashSet<&'a str> {
    HashSet::from_iter(
        possible_anagrams
            .iter()
            .filter(|w| Normalized::new(word).is_anagram(&Normalized::new(w)))
            .copied(),
    )
}

#[derive(Debug)]
struct Normalized(String, String);
impl Normalized {
    fn new(word: &str) -> Self {
        let word = word.to_lowercase();
        let mut charvec: Vec<_> = word.chars().collect();
        charvec.sort();
        Self(word, charvec.into_iter().collect())
    }

    fn is_anagram(&self, other: &Normalized) -> bool {
        self.0 != other.0 && self.1 == other.1
    }
}
