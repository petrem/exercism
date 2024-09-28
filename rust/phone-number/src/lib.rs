// wherefore art thou, try_collect?

pub fn number(user_number: &str) -> Option<String> {
    let cleaned: Vec<char> = user_number
        .chars()
        .filter(|c| !['+', '(', ')', '-', '.', ' '].contains(c))
        .collect();
    cleaned
        .iter()
        .all(|c| c.is_ascii_digit())
        .then(|| match cleaned[..] {
            ['1', '2'..='9', _, _, '2'..='9', ..] if cleaned.len() == 11 => {
                Some(cleaned[1..].iter().collect())
            }
            ['2'..='9', _, _, '2'..='9', ..] if cleaned.len() == 10 => {
                Some(cleaned.into_iter().collect())
            }
            _ => None,
        })
        .flatten()
}

/// Noticed that many community solutions would fail the following test.
/// Sure, without passing such checks, we can simplify the code, but we
/// would not be able to sleep at night ;-)
#[cfg(test)]
//#[cfg(feature = "extra_checks")]
mod tests {
    use super::*;
    #[test]
    fn invalid_when_there_are_extra_nonnumeric_characters() {
        let input = "523-@:!-623-7890";
        let output = number(input);
        assert!(output.is_none());
    }
}
