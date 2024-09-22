// Acronym::Version 3::state machine, immutable

use std::char::ToUppercase;

pub fn abbreviate(phrase: &str) -> String {
    phrase
        .chars()
        .fold(Abbreviator::default(), |abbr, c| abbr.add(c))
        .abbrev
        .into_iter()
        .collect()
}

enum CharKind {
    Upper,
    Word,
    Other,
}

impl From<char> for CharKind {
    fn from(c: char) -> CharKind {
        match c {
            _ if c.is_uppercase() => CharKind::Upper,
            _ if c.is_alphabetic() || c == '\'' => CharKind::Word,
            _ => CharKind::Other,
        }
    }
}

#[derive(Clone, Copy, Default)]
enum AbbrState {
    #[default]
    SeekWordStart,
    InWordLower,
    InWordUpper,
}

use AbbrState::*;

impl AbbrState {
    fn transition(self, c: char) -> (Self, Option<ToUppercase>) {
        match (self, c.into()) {
            (SeekWordStart, CharKind::Upper) => (InWordUpper, Some(c.to_uppercase())),
            (SeekWordStart, CharKind::Word) => (InWordLower, Some(c.to_uppercase())),
            (InWordUpper | InWordLower, CharKind::Other) => (SeekWordStart, None),
            (InWordUpper, CharKind::Word) => (InWordLower, None),
            (InWordLower, CharKind::Upper) => (InWordUpper, Some(c.to_uppercase())),
            (state, _) => (state, None),
        }
    }
}

#[derive(Default)]
struct Abbreviator {
    state: AbbrState,
    abbrev: Vec<char>,
}

impl Abbreviator {
    fn add(self, chr: char) -> Self {
        let (state, emitted) = self.state.transition(chr);
        Self {
            state,
            abbrev: emitted.map_or(self.abbrev.clone(), move |part| {
                self.abbrev.into_iter().chain(part).collect()
            }),
        }
    }
}
