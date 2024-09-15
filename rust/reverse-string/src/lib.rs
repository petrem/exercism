//reverse-string::version 3: another cheaty solution but with grapheme clusters
// (cheaty because we use `.rev()`)
// see https://crates.io/crates/unicode-segmentation

use unicode_segmentation::UnicodeSegmentation;

pub fn reverse(input: &str) -> String {
    UnicodeSegmentation::graphemes(input, true).rev().collect()
}
