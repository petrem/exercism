// Acronym::Version 1::using a state machine

pub fn abbreviate(phrase: &str) -> String {
    let mut fsm = AbbrFSM::new();
    let mut result = String::new();
    for c in phrase.chars() {
        fsm.transition(c, |r| result.push(r.to_ascii_uppercase()));
    }
    result
}

enum CharKind {
    Upper,
    Word,
    Other,
}

impl CharKind {
    // TODO: Is it idiomatic to put functions without self param and not returning Self
    // into an impl? Do these have a name, like "static method" in Python?
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

    fn transition<F>(&mut self, c: char, mut emit: F)
    where
        F: FnMut(char),
    {
        match (&self.state, CharKind::classify(c)) {
            (SeekWordStart, CharKind::Upper) => {
                self.state = InWordUpper;
                emit(c);
            }
            (SeekWordStart, CharKind::Word) => {
                self.state = InWordLower;
                emit(c);
            }
            (InWordUpper | InWordLower, CharKind::Other) => {
                self.state = SeekWordStart;
            }
            (InWordUpper, CharKind::Word) => {
                self.state = InWordLower;
            }
            (InWordLower, CharKind::Upper) => {
                self.state = InWordUpper;
                emit(c);
            }
            _ => {}
        }
    }
}
