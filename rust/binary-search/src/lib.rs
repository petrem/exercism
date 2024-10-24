use std::cmp::Ordering;

pub fn find(array: &[i32], key: i32) -> Option<usize> {
    let mut haystack = array;
    let mut offset: usize = 0;
    while !haystack.is_empty() {
        let middle: usize = haystack.len() / 2;
        haystack = match haystack[middle].cmp(&key) {
            Ordering::Equal => return Some(offset + middle),
            Ordering::Greater => &haystack[..middle],
            Ordering::Less => {
                offset += middle + 1;
                &haystack[middle + 1..]
            }
        }
    }
    None
}
