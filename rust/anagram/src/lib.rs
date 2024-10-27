use std::collections::{HashMap, HashSet};

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
struct Normalized(String, Counter);
impl Normalized {
    fn new(word: &str) -> Self {
        let word = word.to_lowercase();
        let counts = word.chars().collect();
        Self(word, counts)
    }

    fn is_anagram(&self, other: &Normalized) -> bool {
        self.0 != other.0 && self.1 == other.1
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Counter(HashMap<char, usize>);

impl FromIterator<char> for Counter {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        let mut letters = HashMap::new();
        for ch in iter {
            letters
                .entry(ch)
                .and_modify(|counter| *counter += 1)
                .or_insert(1);
        }
        Self(letters)
    }
}
