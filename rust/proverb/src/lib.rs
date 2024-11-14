use std::iter::once;

pub fn build_proverb(words: &[&str]) -> String {
    if let Some(first) = words.first() {
        words
            .windows(2)
            .map(|chunk| {
                if let [w1, w2] = chunk {
                    format!("For want of a {w1} the {w2} was lost.\n")
                } else {
                    panic!("Shouldn't happen, I said windows of two!")
                }
            })
            .chain(once(format!("And all for the want of a {first}.")))
            .collect()
    } else {
        String::new()
    }
}
