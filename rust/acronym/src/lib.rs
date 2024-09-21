// Acronym::Version 2::still using a state machine, but trying a more functional approach.

use std::char::ToUppercase;

// pub fn abbreviate(phrase: &str) -> String {
//     phrase
//         .chars()
//         .into_iter()
//         .fold((AbbrState::default(), Box::new(iter::empty().chain(iter::empty()))),
//               |(state, acc), c| {
//                   let (newstate, emmitted) = state.transition(c);
//                   (newstate, emmitted.map_or(acc, |c| Box::new(acc.chain(iter::once(c)))))
//               })
//         .1
//         .collect()
//         // .filter_map(|c| fsm.transition(c).map(|c| c.to_uppercase().to_string()))
//         // .collect()
// }

pub fn abbreviate(phrase: &str) -> String {
    phrase
        .chars()
        .fold(Abbreviator::new(), |abbr, c| abbr.add(c))
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

#[derive(Default)]
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

struct Abbreviator {
    state: AbbrState,
    abbrev: Vec<char>,
}

impl Abbreviator {
    fn new() -> Self {
        Self {
            state: AbbrState::default(),
            abbrev: vec![],
        }
    }

    fn add(mut self, chr: char) -> Self {
        let (newstate, emitted) = self.state.transition(chr);
        self.state = newstate;
        if let Some(part) = emitted {
            self.abbrev.extend(part);
        }
        self
    }
}
