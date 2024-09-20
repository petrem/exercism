// Acronym::Version 2::still using a state machine, but capturing the result
// with a pipeline.

pub fn abbreviate(phrase: &str) -> String {
    phrase
        .chars()
        .fold((AbbrState::default(), std::iter::empty()), |(state, acc), c| -> {
            let (newstate, emmitted) = state.transition(c);
            (newstate, emmitted.and
        // .filter_map(|c| fsm.transition(c).map(|c| c.to_uppercase().to_string()))
        // .collect()
}

enum CharKind {
    Upper,
    Word,
    Other,
}

impl From<char> for CharKind {
    fn from(c: char) -> CharKind {
        if c.is_uppercase() {
            CharKind::Upper
        } else if c.is_alphabetic() || c == '\'' {
            CharKind::Word
        } else {
            CharKind::Other
        }
    }
}

enum AbbrState {
    SeekWordStart,
    InWordLower,
    InWordUpper,
}

use AbbrState::*;

impl AbbrState {
    fn transition(self, c: char) -> (Self, Option<char>) {
        match (self, c.into()) {
            (SeekWordStart, CharKind::Upper) => (InWordUpper, Some(c)),
            (SeekWordStart, CharKind::Word) => (InWordLower, Some(c)),
            (InWordUpper | InWordLower, CharKind::Other) => (SeekWordStart, None),
            (InWordUpper, CharKind::Word) => (InWordLower, None),
            (InWordLower, CharKind::Upper) => (InWordUpper, Some(c)),
            (state, _) => (state, None),
        }
    }
}

impl Default for AbbrState {
    fn default() -> AbbrState { SeekWordStart }
}
