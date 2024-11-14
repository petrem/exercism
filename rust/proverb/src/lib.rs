pub fn build_proverb(words: &[&str]) -> String {
    let mut proverb = words
        .windows(2)
        .map(|w| {
            if let [w1, w2] = w {
                format!("For want of a {w1} the {w2} was lost.\n")
            } else {
                panic!("I said windows of two!")
            }
        })
        .collect::<String>();
    if let Some(first) = words.first() {
        proverb.push_str(format!("And all for the want of a {}.", first).as_str());
    }
    proverb
}
