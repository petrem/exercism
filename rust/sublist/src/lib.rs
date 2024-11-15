// Sort of using a sort of suffix array

#[derive(Debug, PartialEq, Eq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: std::fmt::Debug + PartialEq + PartialOrd + Ord>(
    l1: &[T],
    l2: &[T],
) -> Comparison {
    if l1 == l2 {
        Comparison::Equal
    } else {
        let sarray1 = SArray::new(l1);
        if sarray1.contains(l2) {
            Comparison::Superlist
        } else {
            let sarray2 = SArray::new(l2);
            if sarray2.contains(l1) {
                Comparison::Sublist
            } else {
                Comparison::Unequal
            }
        }
    }
}

#[derive(Debug)]
struct SArray<'a, T> {
    sarray: Vec<&'a [T]>,
}

impl<'a, T> SArray<'a, T>
where
    T: PartialEq + PartialOrd + Ord,
{
    fn new(list: &'a [T]) -> Self {
        let mut sarray: Vec<&[T]> = Vec::with_capacity(list.len());
        for k in 0..list.len() {
            sarray.push(&list[k..]);
        }
        // Note that this is inefficient, as we sort `list.len().pow(2)` elements
        sarray.sort();
        Self { sarray }
    }

    fn contains(&self, other: &[T]) -> bool {
        match self.sarray.binary_search(&other) {
            _ if self.sarray.is_empty() => false,
            _ if other.is_empty() => true,
            Ok(_) => true,
            Err(next) if next < self.sarray.len() => self.sarray[next].starts_with(other),
            _ => false,
        }
    }
}
