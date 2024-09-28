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
