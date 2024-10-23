pub fn find(array: &[i32], key: i32) -> Option<usize> {
    let mut haystack = array;
    let mut offset: usize = 0;
    while !haystack.is_empty() {
        let middle: usize = haystack.len() / 2;
        haystack = match haystack[middle] {
            v if v == key => return Some(offset + middle),
            v if key < v => &haystack[..middle],
            _ => {
                offset += middle + 1;
                &haystack[middle + 1..]
            }
        }
    }
    None
}
