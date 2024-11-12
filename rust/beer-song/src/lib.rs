use std::{
    fmt::{self, Display, Write},
    iter::{once, repeat},
    ops::Sub,
};

#[rustfmt::skip]
pub fn verse(n: u32) -> String {
    let current = Bottle(n);
    let next = current - 1;
     format!("{} of beer on the wall, {current} of beer.\n\
              {}, {next} of beer on the wall.\n",
             Capitalize(current),
             current.action()
    )
}

pub fn sing(start: u32, end: u32) -> String {
    (end..=start)
        .rev()
        .map(verse)
        .zip(repeat("\n").take((start - end) as usize).chain(once("")))
        .fold(String::new(), |mut acc, (v, term)| {
            let _ = write!(acc, "{v}{term}");
            acc
        })
}

#[derive(Clone, Copy, Debug)]
struct Bottle(u32);

impl Bottle {
    fn action(&self) -> &'static str {
        match self.0 {
            0 => "Go to the store and buy some more",
            1 => "Take it down and pass it around",
            _ => "Take one down and pass it around",
        }
    }
}

impl Display for Bottle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let plural = if self.0 == 1 { "" } else { "s" };
        let count = if self.0 == 0 {
            "no more".to_string()
        } else {
            self.0.to_string()
        };
        write!(f, "{count} bottle{plural}")
    }
}

impl Sub<u32> for Bottle {
    type Output = Self;

    fn sub(self, other: u32) -> Bottle {
        Bottle(self.0.checked_sub(other).unwrap_or(99))
    }
}

// Could have implemented a more or less generic `capitalize()` function, but
// there are so many problems making it really work for any string.

struct Capitalize(Bottle);

impl Display for Capitalize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 .0 == 0 {
            write!(f, "No more bottles")
        } else {
            self.0.fmt(f)
        }
    }
}
