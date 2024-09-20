// Acronym::Version 2::still using a state machine, but capturing the result
// with a pipeline.

pub fn abbreviate(phrase: &str) -> String {
    let mut fsm = AbbrFSM::new();
    phrase
        .chars()
        .filter_map(|c| fsm.transition(c).map(|c| c.to_uppercase().to_string()))
        .collect()
}

enum CharKind {
    Upper,
    Word,
    Other,
}

impl CharKind {
    // TODO: Is it idiomatic to put functions without self param and not returning Self
    // into an impl? Do these have a name, like "static method" in Python?
    // Would it be more typical to rather create a module encapsulating the
    // struct and associated functions?
    fn classify(c: char) -> CharKind {
        if c.is_uppercase() {
            CharKind::Upper
        } else if c.is_alphabetic() || c == '\'' {
            CharKind::Word
        } else {
            CharKind::Other
        }
    }
}

enum AbbrStates {
    SeekWordStart,
    InWordLower,
    InWordUpper,
}
use AbbrStates::*;

struct AbbrFSM {
    state: AbbrStates,
}

impl AbbrFSM {
    fn new() -> Self {
        Self {
            state: SeekWordStart,
        }
    }

    fn transition(&mut self, c: char) -> Option<char> {
        match (&self.state, CharKind::classify(c)) {
            (SeekWordStart, CharKind::Upper) => {
                self.state = InWordUpper;
                Some(c)
            }
            (SeekWordStart, CharKind::Word) => {
                self.state = InWordLower;
                Some(c)
            }
            (InWordUpper | InWordLower, CharKind::Other) => {
                self.state = SeekWordStart;
                None
            }
            (InWordUpper, CharKind::Word) => {
                self.state = InWordLower;
                None
            }
            (InWordLower, CharKind::Upper) => {
                self.state = InWordUpper;
                Some(c)
            }
            _ => None,
        }
    }
}
