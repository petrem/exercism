pub fn translate(input: &str) -> String {
    input
        .split_whitespace()
        .map(translate_word)
        .collect::<Vec<_>>()
        .join(" ")
}

fn translate_word(input: &str) -> String {
    ["a", "e", "i", "o", "u", "xr", "yt"]
        .iter()
        .any(|vowel| input.starts_with(vowel))
        .then_some(format!("{input}ay"))
        .unwrap_or_else(
            || match input.chars().skip(1).position(|c| "aeiouy".contains(c)) {
                None => format!("{input}ay"),
                Some(split_at) => {
                    let (prefix, suffix) = input.split_at(split_at + 1);
                    if let (Some('q'), Some('u')) = (prefix.chars().last(), suffix.chars().next()) {
                        format!("{}{}quay", &suffix[1..], &prefix[..prefix.len() - 1])
                    } else {
                        format!("{suffix}{prefix}ay")
                    }
                }
            },
        )
}
