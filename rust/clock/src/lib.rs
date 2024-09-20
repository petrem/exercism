#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Clock {
    hours: u8,
    minutes: u8,
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let (extra_hours, minutes) = minutes.divmod(60);
        let minutes = minutes as u8;
        let hours = (hours + extra_hours).modulo(24) as u8;
        Clock { hours, minutes }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock::new(self.hours as i32, self.minutes as i32 + minutes)
    }
}

use std::fmt;

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{hour:02}:{minute:02}",
            hour = self.hours,
            minute = self.minutes
        )
    }
}

trait Modulus {
    fn divmod(&self, other: Self) -> (Self, Self)
    where
        Self: Sized;

    /// This should be equivalent to div_euclid
    fn quot(&self, other: Self) -> Self
    where
        Self: Sized,
    {
        let (quot, _) = self.divmod(other);
        quot
    }

    fn modulo(&self, other: Self) -> Self
    where
        Self: Sized,
    {
        let (_, modulus) = self.divmod(other);
        modulus
    }
}

impl Modulus for i32 {
    fn divmod(&self, rhs: i32) -> (i32, i32) {
        let qr @ (q, r) = (self.div_euclid(rhs), self.rem_euclid(rhs));
        if r.signum() == -rhs.signum() {
            (q - 1, r + rhs)
        } else {
            qr
        }
    }
}
