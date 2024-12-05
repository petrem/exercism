// Playing with iterators, this exploits the vertical and horizontal symmetry. It isn't efficient or elegant,
// but Reflect is rather nice, I think.

pub fn get_diamond(c: char) -> Vec<String> {
    assert!(c.is_ascii_uppercase());
    let offset = c as u8 - b'A';
    Reflect::from((0..=offset).map(|i| row(c, i)).collect::<Vec<_>>())
        .iter()
        .cloned()
        .collect()
}

fn row(c: char, pos: u8) -> String {
    let offset = c as u8 - b'A';
    let half = format!(
        "{:>left_pad$}",
        format!(
            "{:right_pad$}",
            (b'A' + pos) as char,
            right_pad = pos as usize + 1
        ),
        left_pad = offset as usize + 1
    );
    Reflect::from(half).iter().collect()
}

struct Reflect<T> {
    items: Vec<T>,
}

impl<T> Reflect<T> {
    fn iter(&self) -> ReflectIterator<T> {
        ReflectIterator {
            items: self.items.as_ref(),
            idx: 0,
            dir: 1,
        }
    }
}

impl<T> From<Vec<T>> for Reflect<T> {
    fn from(items: Vec<T>) -> Self {
        Reflect { items }
    }
}

impl From<String> for Reflect<char> {
    fn from(s: String) -> Self {
        Reflect {
            items: s.chars().collect(),
        }
    }
}

struct ReflectIterator<'a, T> {
    items: &'a [T],
    idx: isize,
    dir: isize,
}

impl<'a, T> Iterator for ReflectIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        let result = self.items.get(self.idx as usize);
        if result.is_some() {
            if self.idx == 0 && self.dir == -1 {
                // ensure we don't get more elements next time
                self.idx = self.items.len() as isize;
            } else {
                if self.idx == (self.items.len() - 1) as isize {
                    self.dir = -1;
                }
                self.idx += self.dir;
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reflect_empty_vec() {
        let v: Vec<i32> = vec![];
        let reflect = Reflect::from(v);
        let expected: Vec<&i32> = vec![];
        assert_eq!(reflect.iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn reflect_singleton_vec() {
        let v: Vec<i32> = vec![10];
        let reflect = Reflect::from(v);
        let expected: Vec<&i32> = vec![&10];
        assert_eq!(reflect.iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn reflect_two_elements_vec() {
        let v: Vec<i32> = vec![1, 2];
        let reflect = Reflect::from(v);
        let expected: Vec<&i32> = vec![&1, &2, &1];
        assert_eq!(reflect.iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn reflect_three_elements_vec() {
        let v: Vec<i32> = vec![1, 2, 3];
        let reflect = Reflect::from(v);
        let expected: Vec<&i32> = vec![&1, &2, &3, &2, &1];
        assert_eq!(reflect.iter().collect::<Vec<_>>(), expected);
    }

    #[test]
    fn reflect_string() {
        let s = String::from("abc");
        let reflect = Reflect::from(s);
        let expected = String::from("abcba");
        assert_eq!(reflect.iter().collect::<String>(), expected);
    }

    #[test]
    fn first_row_for_e() {
        assert_eq!(row('E', 0), "    A    ");
    }

    #[test]
    fn second_row_for_e() {
        assert_eq!(row('E', 1), "   B B   ");
    }

    #[test]
    fn middle_row_for_e() {
        assert_eq!(row('E', 4), "E       E");
    }
}
