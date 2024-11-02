use std::cmp::{max, min};

#[derive(Clone, Copy, Debug)]
pub struct Item {
    pub weight: u32,
    pub value: u32,
}

pub fn maximum_value(max_weight: u32, items: &[Item]) -> u32 {
    let max_weight = max_weight as usize;
    let mut row = vec![0; max_weight + 1];
    for item in items {
        let copy_until = min(item.weight as usize, max_weight + 1);
        row = (0..copy_until)
            .map(|w| row[w])
            .chain(
                (copy_until..=max_weight)
                    .map(|w| max(row[w], row[w - item.weight as usize] + item.value)),
            )
            .collect();
    }
    *row.last().unwrap()
}
